# Pasal Contour

Implementation of a cursor based contour solver routine.
Unit to realize a polygon based contouring routine.

The algorithm uses a maze solving approach, to visit all cells of
by a cursor and register the waypoints along the corridor to wall
transitition.

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
I adopted the DOS-code in 90ies for general use from the third edition of this book
(ISBN-3-528-24543-3).

In technical terms of the time the routine was implemented as a direct
drawing routine to the, in recent considerations, low resoluted PC DOS screen.

The author extract the these parts from the original routine and transformed
the code into a more readable. In addition the algorithm was tested and improved
to prevent hanging nodes and missing "moves" for exotic maze condition.
So checks for border conditions, unique existence and orientation of the
resulting polygons for each height intersected part of the solver are
indroduced.
