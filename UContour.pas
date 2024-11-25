//------------------------------------------------------------------------------
//
//
//------------------------------------------------------------------------------
{
@abstract Contour Algorithm to Convert Scalar Raster Data to a Set of Polygons
@author   Alexander Weidauer alex.weidauer@huckfinn.de
          (C) II/2002 TriplexWare; all rights reserved
@created  December 1990
@lastmod  August 2023

Implementation of a cursor based contour solver routine.
Unit to realize a polygon based contouring routine.

The algorithm is based on a maze solving approach, to visit all cells of
by a cursor and register the waypoints along the corridor to wall transitition.

In an initial state a cursor is placed at the first position with a corridor
to wall transition. The cursor is moved and tests the condition by spin and
touch schema. to keep the left hand on the wall. If the condition was found
the cursor back and moves to the next valid position and the move is registered
as a visited cell and corresponding wall position as a travel path.
The path ends if the cursor reaches the start position and a new corridor to
Wall start condition is found. The routine is repeated until all corridor to
wall are marked as visited. Due to it's "always left hand touch" nature the
registered pathes build a polygon set with outer bounderies, holes,
islands and so on. To prevent hanging nodes and boundary under or  overflow
conditions, the digtal maze uses an oversampling schema.

To build a DEM or ensemble of isolines, the solver provides
the possibility to calculate/ interpolate correct x, y position for the
registered corridor to wall position. To do so, solutions for each intersection
height have to be solved.

The contouring routine was published in germany 1987 by Markus Weber
"Turbo Pascal Tools Practical usage of Turbo Pascal in Nature Science".
I adopted the DOS-code in 90ies for general use from
the third edition of this book (ISBN-3-528-24543-3).

In technical terms of the time the routine was implemented as a direct
drawing routine to the, in recent considerations, low resoluted PC DOS screen.

The author extract the these parts from the original routine and transformed
the code into a more readable. In addition the algorithm was tested and improved
to prevent hanging nodes and missing "moves" for exotic maze condition.
So checks for border conditions, unique existence and orientation of the
resulting polygons for each height intersected part of the solver are
indroduced.
}
unit UContour;

{$mode objfpc}{$H+}
interface

uses
  Classes,
  SysUtils,
  Math,
  UDef,
  UProgress,
  lgVector;

{$DEFINE DEBUG }

const
 { ----------------------------------------------------------------------
   Move directions of the cursor in this order to
   address CCW and CW turning schemata
   ---------------------------------------------------------------------- }

  { Move to West }
  MV_WEST  = 0;

  { Move to South }
  MV_SOUTH = 1;

  { Move to East }
  MV_EAST  = 2;

  { Move to North }
  MV_NORTH = 3;

  { Initiaization }
  MV_INIT = 126;

  ERR_SOLVE_ABORT   = 20;
  ERR_SOLVE_EMPY    = 21;
  ERR_SOLVE_RUNNING = 22;

type


  { Position flags for the digital maze }
  TMazeCellState = (
     mcsVisited,  {< Cell was visited and registered before }
     mcsWall,     {< Cell is part of the wall }
     mcsCorridor, {< Cell is part of the corridor }
     mcsInit      {< For initialization reasons only }
  );

  { Digital maze matrix type }
  TMazeMatrix = array of array of TMazeCellState;

  { Scalar data matrix }
  TDataMatrix = TRealMatrix;

  { Turn dircetion of the Cursor }
  TCursorTurnDirection = (
     { Counter clockwise direction }
     ctdCCW,
     { Clockwise direction }
     ctdCW
  );

  { Position state of the visited cell }
  TCellPositionState = (
     { Initialization state }
     cpsNone,
     { Cell position in the range of the data field }
     cpsInner,
     { Cell position at the Northerm border }
     cpsBorderN,
     { Cell position at the North-Eastern border }
     cpsBorderNW,
     { Cell position at the North-Western border }
     cpsBorderNE,
     { Cell position at the Southern border }
     cpsBorderS,
     { Cell position at the South-Eastern border }
     cpsBorderSW,
     { Cell position at the South-Western border }
     cpsBorderSE,
     { Cell position at the Eastern border }
     cpsBorderW,
     { Cell position at the Western border }
     cpsBorderE,
     { deprecated legacy TODO remove }
     cpsBorderClip
     );

  { Kind of the wall location from the waypoint }
  TCursorTouches = (
      { Cursor touches nothing }
      cstNothing,
      { Cursor touches the plain wall on left side }
      cstWall,
      { Cursor is running into the wall ahead (inner corner) }
      cstMove,
      { Cursor touches the outer corner (left behind) }
      cstOuterCorner,
      { Cursor touches the inner corner (left ahead) }
      cstInnerCorner
  );

 { Simple 2D-vertex with an attribute }
 TVertex = class(TObject)
    { X-Coordinate }
    x: TReal;
    { Y-Coordinate }
    y: TReal;
    { Z-Coordinate }
    z: TReal;
    { Free Attribute }
    attr: TInteger;
    { Cell position state }
    loc:  TCellPositionState;
    { Checks if a vertex is positional the same
      @param   (aVertex: @link (TVertex) - The vertex to compare)
      @returns (@true if coodinates
        are equal and @false if not or
        aVertex is not assigned)
    }
    function IsSamePos(aVertex: TVertex): TBoolean;
 end;

 {  Vector of vertices forming a ring }
 TRing = class(specialize TGObjectVector<TVertex>)
    constructor Create;

    { Get the first path or NIL if the vector is empty. }
    function First: TVertex;

    { Get the last path or NIL if the vector is empty. }
    function Last:  TVertex;

 end;

 { Vector of rings forming a face with polygon exact orientation}
 TFace = class(specialize TGObjectVector<TRing>)
    { Get the first path or NIL if the vector is empty }
    function First: TRing;

    { Get the last path or NIL if the vector is empty }
    function Last:  TRing;

    constructor Create;
 end;


 { Point to describe the relation between a corridor cell and a wall cell }
 TWayPoint = Class(TObject)
   protected

    { Position state of the corresponding maze cell }
    fLocation: TCellPositionState;

    { Orientation of the waypoint }
    fWayDir: TUInt8;

    { Maze matrix column the way point is located }
    fWayCol: TReal;

    { Maze matrix row of the way point is located }
    fWayRow: TReal;

    { Maze cell height of the floor }
    fWayHgt: TReal;

    { Kind of wall the cursor touches on }
    fTouches:  TCursorTouches;

    { Maze matrix column of the wall cell }
    fWallCol: TReal;

    { Maze matrix row of the wall cell }
    fWallRow: TReal;

    { Maze matrix height the wall cell
     (aka) data cell height }
    fWallHgt: TReal;

   public

    { Common creator }
    constructor Create;

    { Check if cell positions are equal
      @param aWayPoint - the way point to compare
      @returns
       @true
       1. if the way point (floor) maze coordinates and move directions
          are equal and
       2. if the wall maze coordinates and the touch direction is equal
       and @false if the condition above is not met or the aWayPoint
       is not assinged.
    }
    function IsEqual(aWayPoint: TWayPoint): TBoolean;

    { Assign aWayPoint to this point }
    procedure Assign(aWayPoint:  TWayPoint);

    { Converts the way point to a @link(TVertex)
      @param aHeight     - a given height between the floor and the wall height
      @param aUseCorners - use the exact corner interpolation or not
      @param aCorrectPos - the cursor position to the corresponding height
                           default is TRUE
      @returns a new memory allocated vertex with the corresponding coordinates
    }
    function ToVertex(aHeight: TReal;
                         aUseCorners: TBoolean;
                         aCorrectPos: TBoolean = TRUE): TVertex;


    { Position state of the corresponding maze cell }
    property Location: TCellPositionState read fLocation;

    { Kind of wall the cursor touches the wall }
    property Touches:  TCursorTouches read fTouches;

    { Orientation of the waypoint }
    property WayDir: TUInt8 read fWayDir;

    { Maze matrix column of the way point }
    property WayCol: TReal  read fWayCol;

    { Maze matrix row of the way point    }
    property WayRow: TReal  read fWayRow;

    { Maze cell height of the floor }
    property WayHgt: TReal  read fWayHgt;

    { Maze matrix column of the wall cell }
    property WallCol: TReal  read fWallCol;

    { Maze matrix row of the wall cell }
    property WallRow: TReal  read fWallRow;

    { Maze matrix height the wall cell (aka) data cell height }
    property WallHgt: TReal  read fWallHgt;

 end;

 { Move path through maze }
 TMazePath =  class( specialize TGObjectVector<TWayPoint>)

    { Get the first waypoint or NIL if the vector is empty }
    function First: TWayPoint;

    { Get the last waypoint or NIL if the vector is empty }
    function Last:  TWayPoint;

    { Get the winding index (area of the enclosed polygon) }
    function WindingIndex: TInteger;

    { Common creator }
    constructor Create;
 end;

 { Ensemble of pathes covering a room including enclosed caverns }
 TMazeTrip =  class( specialize TGObjectVector<TMazePath>)

    { Get the first path or NIL if the vector is empty }
    function First: TMazePath;

    { Get the last path or NIL if the vector is empty }
    function Last:  TMazePath;

    { Common creator }
    constructor Create;
 end;

 { Ensemble of trips covering a complete maze }
 TMazeJourney =  class( specialize TGObjectVector<TMazeTrip>)

    { Get the first trip or NIL if the vector is empty }
    function First: TMazeTrip;

    { Get the last trip or NIL if the vector is empty }
    function Last:  TMazeTrip;

    // Common creator
    constructor Create;
 end;

 { Maze cursor with integer position indexes to address the maze cells }
 TIntCursor = record
   { Column index }
   col: TInteger;
   { Row index   }
   row: TInteger;
   { Orintation coded as @link(MV_WEST MV_SOUTH MV_EAST MV_NORTH) constants. }
   dir: TUInt8;
   { Cursor addresses a data field (TRUE) or is outside(FALSE) }
   isValid: TBoolean;
 end;

 { Maze cursor with real valued position "indexes: to address the data cells
  for interpolation reasons recalculated from the oversampled maze matrix. }
 TRealCursor = record
   { Column index }
   col: TReal;
   { Row index }
   row: TReal;
   { Orintation @link(MV_WEST MV_SOUTH MV_EAST MV_NORTH) constants. }
   dir: TUInt8;
   { Cursor addresses a data field (TRUE) or is outside(FALSE) }
   isValid: TBoolean;
 end;

 { Maze solver events to register some actions for external use }
 TMazeSolverEvent = procedure( Sender:  TObject) of object;

 { Options to feed a maze solver }
 TMazeSolverOption = (
    { Invert the orientation of the maze.

    }
    msoInvertData,
    { Solve the maze with corner correction. }
    msoUseCorners,
    { Solve the maze with border correction
      for positions outside of the data matrix }
    msoCorrectBorder
 );

 { Option set to solve the maze }
 TMazeSolverOptions = set of TMazeSolverOption;

 { Maze - Matrix Solver is a construct use create contour ensemble defined
   from a data matrix. The solver uses the approach to run a cursor through
   a virtual maze. The maze is contsructed by a given cut height defining
   a corridor-wall construct and several options.

     1. If the cut height <= data point the maze becomes the corridor
     2. If the cut height  > data point the maze becomes the wall

   or in the inverted case  (msoInvertData = ON/OFF)

     3. If the cut height > data point the maze becomes the corridor
     4. If the cut height <= data point the maze becomes the wall

    In addition the maze matrix is oversampled and extended corrected
    at the top, left, right and bottom border.
 }
 TMazeSolver =  Class(TObject)
   private
       { The maze matrix repesentation of the data matrix. }
       fMaze: TMazeMatrix;

       { Number of maze columns }
       fMazeCols: TInteger;

       { Number of maze rows }
       fMazeRows: TInteger;

       { Data matrix with real world data }
       fData: TDataMatrix;

       { Number of columns in the data world }
       fDataCols: TInteger;

       { Number of rows in the data world }
       fDataRows: TInteger;

       { Flag if a path was finished (returns to the start point) }
       fFlagPathFinished: TBoolean;

       { Cut height to form the maze is a input parameter) }
       fCutHeight: TReal;

       { Current cursor position maze based }
       fCurCursor: TIntCursor;

       { Start position of a maze solving step }
       fStartCursor: TIntCursor;

       { Currents trip "logger" }
       fCurTrip: TMazeTrip;

       { Current path "logger" }
       fCurPath: TMazePath;

       { Current way point }
       fCurWayPoint: TWayPoint;

       { Flag to use the corner positions during the solving process }
       fUseCorners: TBoolean;

       { Flag to use the border correction approach the solving process }
       fCorrectBorder: TBoolean;

       { Flag to abort solving the problem }
       fAbort: TBoolean;

       { Flag if the maze is solving the problem }
       fSolving: TBoolean;

       { Flag if the maze was solved }
       fSolved: TBoolean;

       { Progress statistics }
       fProgress: TProgress;

       { Event when a maze is created }
       fOnMazeCreated: TNotifyEvent;

       { Event when the solver was cleared for a new solving process }
       fOnSolverCleared: TNotifyEvent;

       { Event when the solver starts solving }
       fOnSolverStarted: TNotifyEvent;

       { Event when solver was aborted }
       fOnSolverAborted: TNotifyEvent;

       { Event when a maze was solved }
       fOnSolverFinished:  TNotifyEvent;

       { Event when a waypoint is added to the current path}
       fOnWayPointAdded: TNotifyEvent;

       { Event when a new path was started }
       fOnPathStarted:   TNotifyEvent;

       { Event when the current path was completed }
       fOnPathFinished:  TNotifyEvent;

       { Event when the solving cursor is turning into or around corners }
       fOnCursorTurned:  TNotifyEvent;

       { Event when the solving cursor is moved to a different cell }
       fOnCursorMoved:  TNotifyEvent;

       { Event when the solving cursor tested the next step for a move }
       fOnNextMoveTested: TNotifyEvent;

       { Test if a data position is located at the border }
       function IsBorder(aCol, aRow: TReal): TBoolean;

       { Turns the current aCursor by 90° in the direction defined by aDir.
         @returns The routin the new turned cursor in and
                  triggers the onCursorTurn event.
       }
       function TurnCursor(aCursor: TIntCursor;
                           aDir: TCursorTurnDirection): TIntCursor;

       { Test if a cursor move in a certain direction is possible
         and staying in the corridor.

         @param    aCursor - the cursor to test.
         @returns  The routine returns @true if the cursor stays on the corridor
                   or reaces a cell the cursor visited before and triggers the
                   onTestMove event.
       }
       function TestMove(aCursor: TIntCursor): TBoolean;

       { Determin the postion state within the data matrix
         @param aCol - Column in the data matrix
         @param aRow - Row in the data matrix
         @returns ( the cell @link(TCellPositionState) where the cursor
          is located is within or outside the data matrix. )
       }
       function MazeToDataInt (aCol, aRow: TInteger): TIntCursor;

       { Coversation of integer based maze cursor data to integer based
         cursor of the incoming data matrix
         @param   aCursor - maze cursor
         @returns integer based cursor of the data incoming data matrix
                    with direction information.
       }
       function MazeToDataInt (aCursor: TIntCursor): TIntCursor;

       { Coversation of integer based maze cursor data to real based
         cursor of the incoming data matrix
         @param   aCursor - maze cursor
         @returns real based cursor of the data incoming data matrix
                  with direction information.
       }
       function MazeToDataReal(aCursor: TIntCursor): TRealCursor;

       { Conversation of maze position data to a integer based cursor of the
         incoming data matrix.
         @param   aCol - Column of the maze position
         @param   aRow - Row of the maze position
         @returns integer based cursor of the data incoming data matrix
                    without direction information )
       }
       function DataCellPosState( aCol, aRow: TReal): TCellPositionState;

       { Starts a new path and stors it in the trip context.
         The initial postion of the cursor is set to @param(aCursor) and
         the path finish flag is set to @false.

         The routine has a implicid role and is called by NextPath.
         So do not use it is not called directly.
       }
       procedure StartPath(aCursor: TIntCursor);

       { Creates an fills the next path through the maze including
         path initialization part (StartPath) by a given start point
         @param(aCursor). The routine test, moves or turns the cursor and
         registers all "corridor-wall-interactions" as waypoints until
         the start point is reached and the finishment of the path is
         flagged and proceded.

         @returns The onStartPath event is trigger at the begin of the routine.
                  During the cursor manipulation process onCursorMove and
                  oCursorTurn are triggered.The onFinishPath event is triggered
                  at the end of the routine.
       }
       procedure NextPath(var aCursor: TIntCursor);

       { Finihes a path. The routine has a implicid role and is called by
         NextPath. So do not use it is not called directly.
       }
       procedure FinishPath;

       { The routine creates a waypoint based on cursors of the
         "corridor-wall-interaction" configuration. The routine is
         called within the NextPath Loop via the RegisterPosition call.

         @param aSourceCursor - Cursor representing the corridor cell
         @param aDesrCursor   - Cursor representing the wall cell
         @param aTouch        - type of wall interaction
         @returns Creates a new waypoint (memory allocation).
       }
       function CreateWayPoint(aSourceCursor,
                               aDestCursor: TRealCursor;
                               aTouch: TCursorTouches): TWayPoint;

       { Registers the cursor position, updates the maze state
         (visited <-> not visited), writes one or more associated waypoints
         or flages the end of the path.
       }
       procedure RegisterPosition(aCursor: TIntCursor);

       procedure SetOnProgressTime(aEventHandler: TNotifyEvent);

       Function  GetOnProgressTime: TNotifyEvent;

    public

       { Common creation call }
       constructor Create;

       { Common destruction call }
       destructor Destroy; override;

       { Clean up a allocated solver Data, Faces, Maze }
       procedure Clear;

       { Extracts the face from an existing solved maze memory allocated.
         A face contains N Rings with correspnding vertices.
       }
       function ExtractFace: TFace;

       { Main routine to create an solve a maze for a certain height cut.
         @param aScalarField - data matrix from which the maze is created
         @param aCutHeight   - height defining which corridor-wall boundary
         @param aOptions     - for the solver operations and maze design
       }
       Function Solve(const aScalarField: TDataMatrix;
                      aCutHeight: TReal;
                      aOptions: TMazeSolverOptions): TResult;


       { Abort the solving routine }
       procedure Abort;

       { Maze matrix created by the height cut and depending
         on msoInvertData or not. }
       property Maze:   TMazeMatrix read fMaze;

       { Number of maze columns }
       property MazeCols: TInteger  read fMazeCols;

       { Number of maze rows }
       property MazeRows: TInteger  read fMazeRows;

       { Data matrix of the real world }
       property Data:     TRealMatrix read fData;

       { Number of data columns }
       property DataCols: TInteger  read fDataCols;

       { Number of data rows }
       property DataRows: TInteger  read fDataRows;

       { Current cursor durening the solve process }
       property CurCursor: TIntCursor   read fCurCursor;

       { Start cursor of the currently solved path }
       property StartCursor: TIntCursor read fStartCursor;

       { Current path ensemble during the solve process }
       property CurTrip: TMazeTrip read fCurTrip;

       { Current path during the solve process }
       property CurPath: TMazePath read fCurPath;

       { Current way point during the solve process }
       property CurWayPoint:    TWayPoint    read fCurWayPoint;

       { Solving state @true if the solver is running, else @false}
       property StateSolving: TBoolean read fSolving;

       { Solved state @true if the maze was solved, else @false }
       property StateSolved:  TBoolean read fSolved;

       { Abortion state @true if the solving process was aborted externally }
       property StateAborted: TBoolean    read fAbort;

       { Event to trigger time statisiscs }
       property OnProgressTime: TNotifyEvent read GetOnProgressTime
                                             write SetOnProgressTime;

       { Notification event if a wap point was added }
       property OnWayPointAdded: TNotifyEvent read  fOnWayPointAdded
                                              write fOnWayPointAdded;

       { Notification event the current path was finished }
       property OnPathFinished: TNotifyEvent read  fOnPathFinished
                                             write fOnPathFinished;

       { Notification event if a new path was started }
       property OnPathStarted: TNotifyEvent read  fOnPathStarted
                                            write fOnPathStarted;

       { Notification event if the current cursor was moved }
       property OnCursorMoved: TNotifyEvent read  fOnCursorMoved
                                            write fOnCursorMoved;

       { Notification event if the current cursor was turned }
       property OnCursorTurned: TNotifyEvent read  fOnCursorTurned
                                             write fOnCursorTurned;

       { Notification event if the next curor move direction is tested }
       property OnNextMoveTested: TNotifyEvent read fOnNextMoveTested
                                               write fOnNextMoveTested;

       { Notification event if the maze was created }
       property OnMazeCreated: TNotifyEvent read  fOnMazeCreated
                                            write  fOnMazeCreated;

       { Notification event if the solving process was started }
       property OnSolverCleared: TNotifyEvent read  fOnSolverCleared
                                              write fOnSolverCleared;

       { Notification event if the solving process was started }
       property OnSolverStarted: TNotifyEvent read  fOnSolverStarted
                                              write fOnSolverStarted;

       { Notification event if the solving process was aborted }
       property  OnSolverAborted: TNotifyEvent read  fOnSolverAborted
                                               write fOnSolverAborted;

       { Notification event if the solving process was was finished }
       property OnSolverFinished: TNotifyEvent read  fOnSolverFinished
                                               write fOnSolverFinished;

       property Progress: TProgress read fProgress;
 end;

 { Conversation of a cursor position to a string TODO TIntCursor.asString }
 function CursorToString(aCursor: TIntCursor): TString;

implementation

function CursorToString(aCursor: TIntCursor): TString;
begin
  Result := 'COL: '+ IntToStr(aCursor.col)
          + ' ROW: '+ IntToStr(aCursor.row)
          + ' DIR: ';
  case aCursor.dir of
      MV_WEST:  Result += 'WEST';
      MV_EAST:  Result += 'EAST';
      MV_SOUTH: Result += 'SOUTH';
      MV_NORTH: Result += 'NORTH';
      else Result := 'STAY';
  end;
end;

function TVertex.IsSamePos(aVertex: TVertex): TBoolean;
begin
  Result := FALSE;
  if aVertex = NIL then exit;
  Result:= ( (aVertex.X = X) and (aVertex.Y = Y) and (aVertex.Z = Z) );
end;

constructor TRing.Create;
begin
  OwnsObjects:=true;
end;

function TRing.First: TVertex;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[0]
end;

function TRing.Last: TVertex;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[count-1]
end;


constructor TFace.Create;
begin
  OwnsObjects:=true;
end;

function TFace.First: TRing;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[0]
end;

function TFace.Last: TRing;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[count-1]
end;


// ============================================================================
// Maze way point implementation
// ============================================================================

constructor TWayPoint.Create;
begin
  inherited;
  fLocation := cpsNone;
  fWayDir := 1;
  fWayCol := 0;
  fWayRow := 0;
  fWayHgt := 0;
  fWallCol := 0;
  fWallRow := 0;
  fWallHgt := 0;
end;

function  TWayPoint.IsEqual(aWayPoint: TWayPoint): TBoolean;
begin
  Result := false;
  if not Assigned(aWayPoint) then exit;
  if aWayPoint.fWayCol <> fWayCol then exit;
  if aWayPoint.fWayRow <> fWayRow then exit;
  if aWayPoint.fWayDir <> fWayDir then exit;
  if aWayPoint.fWallCol <> fWallCol then exit;
  if aWayPoint.fWallRow <> fWallRow then exit;
  if aWayPoint.fTouches <> fTouches then exit;
  Result := true;
end;

procedure  TWayPoint.Assign(aWayPoint: TWayPoint);
begin
  fLocation := aWayPoint.fLocation;
  fTouches := aWayPoint.fTouches;

  fWayDir   := aWayPoint.fWayDir;
  fWayCol   := aWayPoint.fWayCol;
  fWayRow   := aWayPoint.fWayRow;
  fWayHgt   := aWayPoint.fWayHgt;

  fWallCol := aWayPoint.fWallCol;
  fWallRow := aWayPoint.fWallRow;
  fWallHgt := aWayPoint.fWallHgt;
end;

function TWayPoint.ToVertex(aHeight: TReal;
                            aUseCorners: TBoolean;
                            aCorrectPos: TBoolean): TVertex;
var
  lWayCol:  TReal;
  lWallCol: TReal;
  lWayRow:  TReal;
  lWallRow: TReal;
  lDx:  TReal;
  lDy:  TReal;
  lDz:  TReal;
  lSc:  TReal;
  lBorder: TBoolean;
begin
	Result := TVertex.Create;
  lWayCol  := trunc(fWayCol);
  lWayRow  := trunc(fWayRow);
  lWallCol := trunc(fWallCol);
  lWallRow := trunc(fWallRow);

  result.attr := ord(fTouches);
  result.loc  := fLocation;
  result.z    := aHeight;
  lDz  := fWallHgt - fWayHgt;
  if abs(lDz) <  REAL_EPSILON then
    lSc := 0
  else
    lSc := (aHeight-fWayHgt)/lDz;

  lBorder:= ( cpsInner <> fLocation);

  if aCorrectPos then begin
       if aUseCorners or lBorder then begin
         lDx  := fWallCol - fWayCol;
         lDy  := fWallRow - fWayRow;
         result.x := fWayCol+lDx*lSc+0.25;
         result.y := fWayRow+lDy*lSc+0.25;
       end else begin
         lDx  := lWallCol - lWayCol;
         lDy  := lWallRow - lWayRow;
         result.x := lWayCol+lDx*lSc+0.5;
         result.y := lWayRow+lDy*lSc+0.5;
       end;
  end else begin
       if aUseCorners or lBorder then begin
         lDx  := fWallCol - fWayCol;
         lDy  := fWallRow - fWayRow;
         result.x := fWayCol+lDx*lSc+0.25;
         result.y := fWayRow+lDy*lSc+0.25;
       end else begin
         result.x := fWayCol+0.25;
         result.y := fWayRow+0.25;
       end;
  end;
  {$ifopt D+}
   WriteLn('WAY.X: ', lWayCol:2:2, ' WAY.Y: ', lWayRow:2:2,
  ' WALL.X: ', lWallCol:2:2, ' WALL.Y: ',lWallRow:2:2,' X: ',Result.X:2:2,
  ' Y: ', Result.Y:2:2,' Z: ', Result.Z:2:2);
  {$endif}
End;

// ============================================================================
// Maze path implementation
// ============================================================================

constructor TMazePath.Create;
begin
  inherited;
  OwnsObjects:= TRUE;
end;

function TMazePath.First: TWayPoint;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[0]
end;

function TMazePath.Last: TWayPoint;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[Count-1];
end;

function TMazePath.WindingIndex: TInteger;
var
  P, Q: TWayPoint;
begin
  Result := 0;
  Q := Last;
  for P in Self do
      Result += sign(P.WayRow-Q.WayRow) * sign(P.WallCol-Q.WallCol);
end;

// ==========================================================================
// Maze trip implementation
// ==========================================================================

constructor TMazeTrip.Create;
begin
  inherited;
  OwnsObjects:= TRUE;
end;

function TMazeTrip.First: TMazePath;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[0]
end;

function TMazeTrip.Last: TMazePath;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[Count-1];
end;

// ============================================================================
// Maze journey implementation ??? check
// ============================================================================

constructor TMazeJourney.Create;
begin
  inherited;
  OwnsObjects:= TRUE;
end;

function TMazeJourney.First: TMazeTrip;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[0]
end;

function TMazeJourney.Last: TMazeTrip;
begin
  Result := NIL;
  if count < 1 then exit;
  Result := Items[Count-1];
end;

// ============================================================================
// Maze solver implementation
// ============================================================================

Constructor TMazeSolver.Create;
begin
   inherited;
   fCurTrip    := TMazeTrip.Create;
   fCurWayPoint:= NIL;
   fCurPath    := NIL;
   fUseCorners := false;
   fProgress := TProgress.Create;
end;

destructor TMazeSolver.Destroy;
begin
  SetLength(fMaze, 0, 0);
  fCurTrip.Free;
  fProgress.Free;
  inherited;
end;

procedure TMazeSolver.Clear;
begin
  fMazeCols:= 0; fMazeRows:= 0;
  fDataCols:= 0; fDataRows:= 0;
  with fCurCursor do begin
    col:=-1; row:=-1; isValid:= false;
  end;
  with fStartCursor do begin
    col:=-1; row:=-1; isValid:= false;
  end;
  fFlagPathFinished:= FALSE;
  fCutHeight := REAL_NAN;
  SetLength(fMaze, 0, 0);
  fCurPath:= NIL;
  fCurWayPoint := NIL;
  fCurTrip.Clear;
  fSolved  := FALSE;
  fSolving := FALSE;
  fAbort   := FALSE;
  if Assigned(fOnSolverCleared) then fOnSolverCleared(Self);
end;

function TMazeSolver.IsBorder(aCol, aRow: TReal): TBoolean;
begin
  Result := TRUE;
  if aCol < 0 then exit;
  if aCol >= fDataCols then exit;
  if aRow < 0 then exit;
  if aRow >= fDataRows then exit;
  Result := FALSE;
end;

function TMazeSolver.MazeToDataInt(aCol, aRow: TInteger): TIntCursor;
begin
  with Result do begin
    col := aCol div 2 - 1;
    row := aRow div 2 - 1;
    dir := MV_INIT;
    isValid:= not IsBorder(col, row);
  end;
end;

function TMazeSolver.MazeToDataInt(aCursor: TIntCursor): TIntCursor;
begin
  with Result do begin
    col := aCursor.col div 2 - 1;
    row := aCursor.row div 2 - 1;
    dir := aCursor.dir;
    isValid:= not IsBorder(col, row);
  end;
end;

function TMazeSolver.MazeToDataReal(aCursor: TIntCursor): TRealCursor;
begin
  with Result do begin
    col := aCursor.col / 2 - 1;
    row := aCursor.row / 2 - 1;
    isValid:= not IsBorder(col, row);
    dir := aCursor.dir;
  end;
end;

function TMazeSolver.DataCellPosState(aCol, aRow: TReal): TCellPositionState;

var
  lN, lS, lW, lE: TBoolean;
begin
  Result := cpsNone;
  lN := (aRow < 0); lS := ( aRow >= fDataRows);
  lW := (aCol < 0); lE := ( aCol >= fDataCols);
  if lN and lW then begin Result := cpsBorderNW; exit; end;
  if lS and lW then begin Result := cpsBorderSW; exit; end;
  if lN and lE then begin Result := cpsBorderNE; exit; end;
  if lS and lE then begin Result := cpsBorderSE; exit; end;
  if lS then begin Result := cpsBorderS; exit; end;
  if lN then begin Result := cpsBorderN; exit; end;
  if lE then begin Result := cpsBorderE; exit; end;
  if lW then begin Result := cpsBorderW; exit; end;
  Result := cpsInner;
end;

function TMazeSolver.TestMove(aCursor: TIntCursor): TBoolean;
var
   lTest: TMazeCellState;
   lCol, lRow: TInteger;
begin
  lTest := mcsInit;
  lCol:= aCursor.col;
  lRow:= aCursor.row;
  case ord(aCursor.dir) of
      MV_WEST:  lTest := fMaze[lCol-1,lRow];
      MV_SOUTH: lTest := fMaze[lCol,lRow+1];
      MV_EAST:  lTest := fMaze[lCol+1,lRow];
      MV_NORTH: lTest := fMaze[lCol,lRow-1];
  end;
  Result :=( ( lTest = mcsVisited ) or (lTest = mcsCorridor) );
  if Assigned(fOnNextMoveTested) then fOnNextMoveTested(Self);
end;


Function TMazeSolver.CreateWayPoint(aSourceCursor,
                                    aDestCursor: TRealCursor;
                                    aTouch: TCursorTouches): TWayPoint;
begin

  Result         := TWayPoint.Create;
  Result.fWayDir := aSourceCursor.dir;
  Result.fWayCol := aSourceCursor.col;
  Result.fWayRow := aSourceCursor.row;

  Result.fWallCol:= aDestCursor.col;
  Result.fWallRow:= aDestCursor.row;

  Result.fTouches:= aTouch;

  Result.fLocation:=DataCellPosState(aSourceCursor.col,
                                     aSourceCursor.row);


  if Result.fLocation <> cpsInner then
     Result.fWayHgt:= fCutHeight
  else
     Result.fWayHgt:= fData[trunc(aSourceCursor.col),
                            trunc(aSourceCursor.row)];

  if IsBorder(aDestCursor.col, aDestCursor.row)  then
     Result.fWallHgt:= REAL_NAN
  else
     Result.fWallHgt:= fData[trunc(aDestCursor.col),
                             trunc(aDestCursor.row)];
end;

procedure TMazeSolver.StartPath(aCursor: TIntCursor);
begin
  // Initial Condition
  fCurTrip.Add(TMazePath.Create);
  fCurPath:=fCurTrip.Last;
  fStartCursor := aCursor;
  fFlagPathFinished := false;
  if Assigned(fOnPathStarted) then fOnPathStarted(Self);
end;

procedure TMazeSolver.FinishPath;
begin
  if Assigned(fOnPathFinished) then fOnPathFinished(self);
  fStartCursor.col := -1; fStartCursor.row := -1;
  fStartCursor.isValid:= FALSE; fStartCursor.dir := MV_INIT;
end;

procedure TMazeSolver.RegisterPosition(aCursor: TIntCursor);
var
   lSourceCursor: TRealCursor;
   lDestCursor:   TRealCursor;
   lMoveCursor:   TIntCursor;
   lWallCursor:   TIntCursor;
   lCornerCursor: TIntCursor;
   lWallCase,
   lOuterCorner,
   lInnerCorner:  TBoolean;
   lWayPoint:       TWayPoint;
begin
  // Register the vist of the cell in the Maze matrix
  fMaze[aCursor.col, aCursor.row] := mcsVisited;

  // Solve the CurCursor for then next move and the cell position 90° from
  // of the current direction where the "wall" should be...
  lMoveCursor := aCursor;
  lWallCursor := aCursor;
  case aCursor.dir of
      MV_WEST:  begin dec(lMoveCursor.col); inc(lWallCursor.row); end;
      MV_SOUTH: begin inc(lMoveCursor.row); inc(lWallCursor.col)  end;
      MV_EAST:  begin inc(lMoveCursor.col); dec(lWallCursor.row)  end;
      MV_NORTH: begin dec(lMoveCursor.row); dec(lWallCursor.col)  end;
  end;

  // Check if we have contact but moving along the wall
  lWallCase := (fMaze[lMoveCursor.col, lMoveCursor.row] <> mcsWall)
           and (fMaze[lWallCursor.col, lWallCursor.row] =  mcsWall);

  // Check we have lost the contact so we have an outer corner
  lOuterCorner := (fMaze[lMoveCursor.col, lMoveCursor.row] <> mcsWall)
               and (fMaze[lWallCursor.col, lWallCursor.row] <> mcsWall);

  // Check we have contact but are locked in a inner corner
  lInnerCorner := (fMaze[lMoveCursor.col, lMoveCursor.row]  = mcsWall)
               and (fMaze[lWallCursor.col, lWallCursor.row] = mcsWall);

  // Calculat the current CurCursor position in the real world matrix
  lSourceCursor := MazeToDataReal(aCursor);

  // -------------------------------------------------------------------------
  // Simple part of the destination calculation
  // -------------------------------------------------------------------------

  // If we have contact to the wall, we have to resister our position
  // and the destination cell is the guessed wall cell 90° from our position
  if lWallCase or lInnerCorner then  begin
     lDestCursor  := MazeToDataReal(lWallCursor);
     lWayPoint:= CreateWayPoint(lSourceCursor, lDestCursor, cstWall);
     fCurPath.Add(lWayPoint);
  end
  else

  // If we have no contact to the wall, we have to resister the
  // corner position as destination
  if fUseCorners then begin
    if lOuterCorner then begin
       lCornerCursor:= aCursor;
       case aCursor.dir of
           MV_WEST:  begin inc(lCornerCursor.col); inc(lCornerCursor.row) end;
           MV_SOUTH: begin inc(lCornerCursor.col); dec(lCornerCursor.row) end;
           MV_EAST:  begin dec(lCornerCursor.col); dec(lCornerCursor.row) end;
           MV_NORTH: begin dec(lCornerCursor.col); inc(lCornerCursor.row) end;
       end;
       lDestCursor := MazeToDataReal(lCornerCursor);

       // Store the CurWayPoint
       lWayPoint:= CreateWayPoint(lSourceCursor, lDestCursor, cstOuterCorner);
       fCurPath.Add(lWayPoint);
       if Assigned(fOnWayPointAdded) then fOnWayPointAdded(self);
     end;
   end;
   // ------------------------------------------------------------------------
   // Tricky part of the destination calculation for inner coners
   // ------------------------------------------------------------------------

   // In case of an inner corner multiple "Source-Destination" adresses
   // must be inserted in the right order.
   // 1. If an valid cell beyond the corner exists this desitnation
   // 2. The cell which is now marked as visited and registerd by the
   //    guessed and valid wall position, but the cell in the succeding
   //    position in move direction has to be registered too
   if lInnerCorner then begin
      if fUseCorners then begin
        // Solve the corner cell
        lCornerCursor:= aCursor;
        case aCursor.dir of
            MV_WEST:  begin dec(lCornerCursor.col); inc(lCornerCursor.row) end;
            MV_SOUTH: begin inc(lCornerCursor.row); inc(lCornerCursor.col) end;
            MV_EAST:  begin inc(lCornerCursor.col); dec(lCornerCursor.row) end;
            MV_NORTH: begin dec(lCornerCursor.row); dec(lCornerCursor.col) end;
        end;
        // Diagonal cell is present so we have to insert the cell
        if fMaze[lCornerCursor.col, lCornerCursor.row] = mcsWall then begin
          lDestCursor:= MazeToDataReal(lCornerCursor);

          // Store the CurWayPoint
          lWayPoint:= CreateWayPoint(lSourceCursor, lDestCursor, cstInnerCorner);
          fCurPath.Add(lWayPoint);
          if Assigned(fOnWayPointAdded) then fOnWayPointAdded(self);
        end;
      end;

      // Finally add the turn position (inner cormer) of the current cell
      // The destination cell is defined by the move CurCursor.
      lDestCursor:= MazeToDataReal(lMoveCursor);

      // Store the CurWayPoint
      lWayPoint:= CreateWayPoint(lSourceCursor, lDestCursor, cstMove);
      fCurPath.Add(lWayPoint);
      if Assigned(fOnWayPointAdded) then fOnWayPointAdded(self);

   end; // Eof Outer Corner

   // Update he current Vertex
   fCurWayPoint:=lWayPoint;

   fFlagPathFinished:= ( ( aCursor.col = fStartCursor.col)
                     and ( aCursor.row = fStartCursor.row) );
   if fFlagPathFinished then FinishPath;

end;

function TMazeSolver.TurnCursor(aCursor: TIntCursor; aDir: TCursorTurnDirection): TIntCursor;
begin
   Result := aCursor;
   case aDir of
       ctdCCW : Result.dir := (aCursor.dir + 1) and 3;
       ctdCW  : Result.dir := (aCursor.dir + 3) and 3;
   end;
   if Assigned(fOnCursorTurned) then fOnCursorTurned(Self);
end;

// TODO -> move aDir into the cursor and
procedure TMazeSolver.NextPath(var aCursor: TIntCursor);
begin

   // The routine is fCurCursor based aCursor := fCursor
   StartPath(aCursor);

   // If the polygon was not finished
   while not (fFlagPathFinished or fAbort) do begin

     fProgress.Update(0);

     // Turn the CurCursor CCW, back from the current move direction
     aCursor := TurnCursor(aCursor, ctdCCW);

     // Turn the CurCursor CW until he finds the next move direction
     while not TestMove(aCursor) do begin
        aCursor := TurnCursor(aCursor, ctdCW);
     end;

     // Register the new test position
     if not fFlagPathFinished then begin

       // Move the CurCursor to the test position
       case aCursor.dir of
           MV_WEST:  dec(aCursor.col);
           MV_SOUTH: inc(aCursor.row);
           MV_EAST:  inc(aCursor.col);
           MV_NORTH: dec(aCursor.row);
       end;

       // Register the position
       RegisterPosition(aCursor);

       // Call the movement event
       if Assigned(fOnCursorMoved) then fOnCursorMoved(Self);
     end; // if not fFlagPathFinished
   end; // while not fFlagPathFinished
   fProgress.Update(0);
   if not fAbort Then FinishPath;
end;

function TMazeSolver.ExtractFace: TFace;
var
   lCurPoint:  TWayPoint;
   lPrevPoint:   TWayPoint;
   lNextPoint:   TWayPoint;
   lPath:   TMazePath;
   lRing:   TRing;
   lAddVertex:    TVertex;
   lCurVertex:    TVertex;
   lIsCorrectVertex:    TBoolean;
   lIx:     TInteger;
begin
   Result := TFace.Create;
   for lPath in fCurTrip do begin
     lRing := TRing.Create;
     lCurVertex := NIL;
     for lIx := 0 to lPath.Count-1 do begin

         // --- Assemble point sequence ---------------------------------------
         lCurPoint := lPath[lIx];
         if lCurPoint = lPath.First then begin
            lPrevPoint := lPath.Last;
            lNextPoint := lPath[lIx+1];
         end
         else
         if lCurPoint = lPath.Last then begin
            lNextPoint := lPath.First;
            lPrevPoint := lPath[lIx-1];
         end
         else begin
           lPrevPoint := lPath[lIx-1];
           lNextPoint := lPath[lIx+1];
         end;

         // --- Border correction mode ---------------------------------------
         if fCorrectBorder then
            lIsCorrectVertex:= TRUE
         else
            lIsCorrectVertex := (
                   (lPrevPoint.fLocation  = cpsInner)
              and  (lCurPoint.fLocation   = cpsInner)
              and  (lNextPoint.fLocation  = cpsInner)
            );

         // --- Vertex add procexure -----------------------------------------
         lAddVertex := lCurPoint.ToVertex(fCutHeight,
                                          fUseCorners,
                                          lIsCorrectVertex);

         if lAddVertex.IsSamePos(lCurVertex) then begin
           lAddVertex.Free;
         end else begin
           lRing.Add(lAddVertex);
           lCurVertex := lAddVertex;
         end;
         lPrevPoint:=lCurPoint;
     end;
     Result.Add(lRing)
   end;
end;

// --------------------------------------------------------------------------
procedure  TMazeSolver.Abort;
begin
  fAbort := TRUE;
end;

procedure TMazeSolver.SetOnProgressTime(aEventHandler: TNotifyEvent);
begin
  fProgress.OnUpdateTime := aEventHandler;
end;

Function  TMazeSolver.GetOnProgressTime: TNotifyEvent;
begin
  Result := fProgress.OnUpdateTime;
end;

Function TMazeSolver.Solve(
                        const aScalarField: TDataMatrix;
                        aCutHeight: TReal;
                        aOptions: TMazeSolverOptions): TResult;
var
   lCol, lRow: TInteger;
   lCursor:    TIntCursor;
   lCellState: TMazeCellState;
   lInvert:    TBoolean;
begin
  if fSolving then begin
    ResError('Solver is already running!',
             'Use abort to stop the process.',
             ERR_SOLVE_RUNNING);
    exit;
  end;
  if (Length(aScalarField) = 0) or
     (Length(aScalarField[0]) = 0) then begin
      ResError('Incoming data array is empty!',
               '', ERR_SOLVE_EMPY);
      exit;
  end;
  fSolving:= TRUE;
  if Assigned(fOnSolverStarted) then fOnSolverStarted(self);
  Result := ResError('Solver is running!');
  fSolved := FALSE;
  fData := aScalarField;
  fCutHeight := aCutHeight;
  fDataCols:= Length(fData);
  fDataRows:= Length(fData[0]);
  fUseCorners:= (msoUseCorners in aOptions);
  fCorrectBorder :=( msoCorrectBorder in aOptions);

  lInvert:= (msoInvertData in aOptions);

  { Initialize the binary search field by oversampling it, to prevent
    singularities for the nodes }
  fMazeCols := 2 * fDataCols + 4;
  fMazeRows := 2 * fDataRows + 4;
  SetLength(fMaze, fMazeCols, fMazeRows);
  fProgress.Init(Length(fMaze) * Length(fMaze[0]));
  fProgress.Update(0);

  { Construct the maze }
  for lCol:= Low(fMaze) to High(fMaze) do
    for lRow:= Low(fMaze[lCol]) to High(fMaze[lCol]) do begin
        lCursor  := MazeToDataInt(lCol, lRow);
        lCellState := mcsCorridor;
        if lCursor.isValid then begin
          if lInvert then begin
             if fData[lCursor.col, lCursor.row] <= aCutHeight then
                lCellState := mcsWall;
          end else begin
             if fData[lCursor.col, lCursor.row]  > aCutHeight then
                lCellState := mcsWall;
          end;
        end;
        fMaze[lCol,lRow]:= lCellState;
  end;

  { Notify maze created }
  if Assigned(fOnMazeCreated) then fOnMazeCreated(self);;

  // Iterate over all fields of the masks until no transititions cells
  // with then mcsWall and mcsCorridor exists. Each NextPath calculation
  // will provide in a mcsCorridor flag around the mcsWall flagged area.

  for lCol:= Low(fMaze) to High(fMaze) do begin
    for lRow:= Low(fMaze[lCol]) to High(fMaze[lCol]) do begin

         // TODO set maze solving percentage and ETA

         // Notify solver was aborted
         if fAbort then begin
           fSolving := false;
           if Assigned(fOnSolverAborted) then fOnSolverAborted(self);
           Result := ResError('Solver was aborted!','',ERR_SOLVE_ABORT);
           fProgress.Update(0,TRUE);
           exit;
         end;

        // Found a  transition in Northern direction
        if (fMaze[lCol, lRow]   = mcsWall) and
           (fMaze[lCol, lRow-1] = mcsCorridor) then begin

         // Create a North oriented  CurCursor
         fCurCursor.col := lCol; fCurCursor.row := lRow-1; fCurCursor.dir := 3;

         { Notify cursor moved }
         if Assigned(fOnCursorMoved) then fOnCursorMoved(self);
         // Curse over these polygon
         NextPath(fCurCursor);
       end; // if found transitition
        fProgress.Update(1);
    end; // for lRow
  end;

  // Notify the solver finishment
  fProgress.Update(0, TRUE);
  if Assigned(fOnSolverFinished) then fOnSolverFinished(Self);
  // Set final states
  Result  := ResOK;
  fSolving:= FALSE;
  fSolved := TRUE;
end;

end.

