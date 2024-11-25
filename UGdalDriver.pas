unit UGdalDriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics, Forms,
  BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRAReadTiff,

  Math,

  UDef, ULogger, gdal, gdalcore;

const
  GTIFF_FMT = 'GTiff';
  MAX_DIGIT = 3;

type

  TGdalAffineTransform = array [0..5] of TReal;

  TGdalTifDriver = class(TObject)
      private
         mDriver:    GDALDriverH;
         mDataset:   GDALDatasetH;
         mLogger:    TLogger;
         mGdalTrfmFwd: TGdalAffineTransform;
         mTrfmFwd:   TAffineTransform;
         mTrfmInv:   TAffineTransform;
         mWX1, mWX2, mWY1, mWY2: TReal;
         mIX1, mIX2, mIY1, mIY2: TInteger;
         mImgWidth:  Integer;
         mImgHeight:   Integer;
         mImgOffsX, mImgOffsY : TInteger;
         mImgNumBand:     Integer;

         // GDAL Interfaces
         mLBand:     array of GDALRasterBandH;
         mLType:     array of GDALDataType;
         mLSize:     array of LongInt;
         mCanTrfm:   TBoolean;

         function    GdalTrfmImageWorld(aCol, aRow:TInteger; var oX, oY: TReal): TResult;
         function    GdalTrfmWorldImage(aX, aY:TReal; var oCol, oRow: TInteger): TResult;
      public
        constructor Create(aLog:TLogger);
        destructor  Destroy; override;
        function    Init:TResult;
        function    ScreenTrfmImageWorld(aCol, aRow:TInteger; var oX, oY: TReal): TResult;
        function    ScreenTrfmWorldImage(aX, aY:TReal; var oCol, oRow: TInteger): TResult;
        function    LoadTile(aWX1, aWY1, aWX2, aWY2: TReal;
                             var oBitmap: TBGRABitmap): TResult;
        function    OpenSource(aName: String; aMode: GDALAccess): TResult;
        function    SetCheckTransform(const aTrfmFwd, aTrfmInv: TAffineTransform): TResult;
        function    CloseSource: TResult;
        function    IsDataSource: Boolean;

      property IWidth: Integer read mImgWidth;
      property IHeight: Integer read mImgHeight;
  end;


implementation

 constructor TGdalTifDriver.Create(aLog:TLogger);
 begin
   inherited create;
   mLogger  := aLog;
   mDriver  := NIL;
   mDataset := NIL;
 end;

 destructor  TGdalTifDriver.Destroy;
 begin
   inherited destroy;
 end;

 function TGdalTifDriver.CloseSource: TResult;
 begin
   Result := ResOK('Close GDAL dataset!');
   if mDataset = NIL then exit;
   GDALClose(mDataset);
   setLength(mLBand, 0);
   setLength(mLType, 0);
   setLength(mLSize, 0);
   // mLogger.InfoLN('IMAGE.DRIVER: Close dataset is OK.');
   mDataset:=NIL;
 end;

 function  TGdalTifDriver.IsDataSource: Boolean;
 begin
   result := false;
   if (mDataset = NIL) then exit;
   result := true;
 end;

 function TGdalTifDriver.SetCheckTransform(const aTrfmFwd, aTrfmInv: TAffineTransform): TResult;
 var
   lDX,   lDY: TReal;
   lGX,   lGY: TReal;
   lDCol, lDRow: TInteger;
   lGCol, lGRow: TInteger;
 begin
   Result := ResOK;
   if not IsDataSource then begin
     Result := ResError('Image not initialized!');
     exit;
   end;

   if IsNan(mGdalTrfmFwd[0]) or IsNan(mGdalTrfmFwd[1]) or
      IsNan(mGdalTrfmFwd[2]) or IsNan(mGdalTrfmFwd[3]) or
      IsNan(mGdalTrfmFwd[4]) or IsNan(mGdalTrfmFwd[5])
   then begin
     Result := ResError('Image transform not initialized!');
     exit;
   end;

   if IsNan(aTrfmFwd[1,1]) or IsNan(aTrfmFwd[1,2]) or IsNan(aTrfmFwd[1,3]) or
      IsNan(aTrfmFwd[2,1]) or IsNan(aTrfmFwd[2,2]) or IsNan(aTrfmFwd[2,3])
   then begin
     Result := ResError('Forward transform not initialized!');
     exit;
   end;

   if IsNan(aTrfmInv[1,1]) or IsNan(aTrfmInv[1,2]) or IsNan(aTrfmInv[1,3]) or
    IsNan(aTrfmInv[2,1]) or IsNan(aTrfmInv[2,2]) or IsNan(aTrfmInv[2,3])
   then begin
     Result := ResError('Inverse transform not initialized!');
     exit;
   end;

   mTrfmFwd := aTrfmFwd; mTrfmInv := aTrfmInv;
   // Cross Check of DB sorted and Image Store Trfm
   ScreenTrfmImageWorld(0, 0, lDX, lDY);
   ScreenTrfmWorldImage(lDX, lDY, lGCol, lGRow);
   GdalTrfmImageWorld(0, 0,   lGX, lGY);
   GdalTrfmWorldImage(lGX, lGY, ldCol, lDRow);
   if (abs(lDCol-lGCol) > 2) or ( abs(lDRow - lGRow) > 2) then begin
     Result := ResError('Database and image transforms differ!');
     exit;
   end;

 end;

 function TGdalTifDriver.ScreenTrfmImageWorld(aCol, aRow:TInteger; var oX, oY: TReal): TResult;
 begin
   Result := ResOK();
   ox := mTrfmFwd[1,1]*aCol+mTrfmFwd[1,2]*aRow + mTrfmFwd[1,3];
   oy := mTrfmFwd[2,1]*aCol+mTrfmFwd[2,2]*aRow + mTrfmFwd[2,3];
 end;

 function TGdalTifDriver.ScreenTrfmWorldImage(aX, aY:TReal; var oCol, oRow: TInteger): TResult;
 begin
   Result := ResOK();
   oCol := round(mTrfmInv[1,1]*aX+mTrfmInv[1,2]*aY+mTrfmInv[1,3]);
   oRow := round(mTrfmInv[2,1]*aX+mTrfmInv[2,2]*aY+mTrfmInv[2,3]);
 end;

 function TGdalTifDriver.GdalTrfmImageWorld(aCol, aRow:TInteger;
                                            var oX, oY: TReal): TResult;
 begin
   Result := ResOK();
   oX := mGdalTrfmFwd[0] + mGdalTrfmFwd[1] * aCol + mGdalTrfmFwd[2] * aRow;
   oY := mGdalTrfmFwd[3] + mGdalTrfmFwd[4] * aCol + mGdalTrfmFwd[5] * aRow;
 end;

 function TGdalTifDriver.GdalTrfmWorldImage(aX, aY:TReal;
                                            var oCol, oRow: TInteger): TResult;
 var
   lDiv: TReal;
 begin

   Result := ResOK();
   lDiv := (mGdalTrfmFwd[2]*mGdalTrfmFwd[4]-mGdalTrfmFwd[1]*mGdalTrfmFwd[5]);
   if (lDiv < REAL_EPSILON * 2) then begin
     Result := ResError('Invalid scaling factor!');
     oCol := -1; oRow := -1;
     exit;
   end;
   oCol := round(-(mGdalTrfmFwd[2]*(mGdalTrfmFwd[3]-aY)
                 + mGdalTrfmFwd[5]*aX-mGdalTrfmFwd[0]*mGdalTrfmFwd[5])/lDiv);
   oRow := round( (mGdalTrfmFwd[1]*(mGdalTrfmFwd[3]-aY)
                 + mGdalTrfmFwd[4]*aX-mGdalTrfmFwd[0]*mGdalTrfmFwd[4])/lDiv);
 end;

 function TGdalTifDriver.OpenSource(aName: String; aMode: GDALAccess): TResult;
 var
   ix : Integer;
   lCplErr : CPLErr;
 begin
   result := ResOK;
   mCanTrfm:= FALSE;

   if mDataset <> NIL then CloseSource;
   mDataset := GDALOpen( PChar(aName), aMode);
   if mDataset = NIL then begin
     result := ResError('IMAGE.DRIVER: Unknown datasource '+aName);
     exit;
   end;
   mGdalTrfmFwd[0] := 0; mGdalTrfmFwd[1] := 1;
   mGdalTrfmFwd[2] := 0;
   mGdalTrfmFwd[3] := 0;
   mGdalTrfmFwd[4] := 0; mGdalTrfmFwd[5] := 1;

   lCplErr:= GDALGetGeoTransform( mDataset, @mGdalTrfmFwd );
   if lCplErr <> CE_None then begin
     result := ResError('IMAGE.DRIVER: Unknown transform for datasource '+aName);
     exit;
   end;
   mImgWidth  := GDALGetRasterXSize(mDataset);
   mImgHeight := GDALGetRasterYSize(mDataset);
   mImgNumBand     := GDALGetRasterCount(mDataset);
   if mImgNumBand > 4 then begin
     result := ResError('Max. 4 byte channels are supported');
     mLogger.ErrorFmt('Max. 4 byte channels are supported',[]);
     exit;
   end;

   SetLength(mLBand, mImgNumBand);
   SetLength(mLType, mImgNumBand);
   SetLength(mLSize, mImgNumBand);

   for ix :=0 to mImgNumBand-1 do begin
     mLBand[ix] := GDALGetRasterBand( mDataset, ix+1 );
     mLType[ix] := GDALGetRasterDataType(mLBand[ix]);
     mLSize[ix] := GDALGetDataTypeSize(mLType[ix]);
     if mLSize[ix] <> 8 then begin
       result := ResError('Only byte channels are supported');
       mLogger.ErrorFmt('Only byte channels are supported %d',[mLSize[ix]]);
       exit;
     end;
   end;
   mLogger.InfoLN('Open Image: '+aName+' is OK.');
   // mLogger.InfoFmt('IMAGE.DRIVER: Image %dx%d bands %d ',[mImgWidth, mImgHeight, mImgNumBand]);
 end;

 function TGdalTifDriver.LoadTile(aWX1, aWY1, aWX2, aWY2: TReal;
                                  var oBitmap : TBGRABitmap): TResult;
 var
   ib, ic, ir: Integer;

   lIoTransfer  : array of Pointer;
   lOx, lOy, lWdt, lHgt: Integer;
   lBGRARowPointer: PBGRAPixel;
   lGDALRowPointer:  array of PByte;
   lSwapReal: TReal;
 begin

   // Initialize The canvas an the
   // mBuffer   -- corresponding image buffer on the FPC side and the
   // lIoTransfer  -- gdal transfer buffer
    if not Assigned(oBitmap) then
         oBitmap := TBGRABitmap.Create(10, 10);

   // Synchronize Bitmap and Canvas Sizes
   lWdt := oBitmap.Width; lHgt := oBitmap.Height;

   // if aWX1 > aWX2 then begin lSwapReal := aWX1; aWX1 := awX2; aWX2 := lSwapReal; end;
   // if aWY1 > aWY2 then begin lSwapReal := aWY1; aWY1 := awY2; aWY2 := lSwapReal; end;

   ScreenTrfmWorldImage(aWX1, aWY1, mIX1, mIY1);
   ScreenTrfmWorldImage(aWX2, aWY2, mIX2, mIY2);
   mWX1 := aWX1; mWY1 := aWY1; mWX2 := aWX2; mWY2 := aWY2;

   lWdt := (mIX2 - mIX1); lHgt := abs(mIY1 - mIY2);
   mImgOffsX  := min(mIX1, mIX2); mImgOffsY := min(mIY1, mIY2);

   // We have an underflow in the image  transfer buffer buffer
   if mImgOffsX < 0 then lOx := -mImgOffsX else lOx := 0;
   if mImgOffsY < 0 then lOy := -mImgOffsY else lOy := 0;

   lWdt := oBitmap.Width-lOx; lHgt := oBitmap.Height-lOy;
   if mImgOffsX+lOx+lWdt >= mImgWidth  then lWdt:= mImgWidth-mImgOffsX-lOx-1;
   if mImgOffsY+lOY+lHgt >= mImgHeight then lHgt:= mImgHeight-mImgOffsY-lOY-1;


   // Prepare the transfer buffer pointes GDAL Read -- BGRA write
   oBitmap.FillRect(0, 0, oBitmap.Width, oBitmap.Height, BGRABlack, dmSet);
   SetLength(lIoTransfer,     mImgNumBand);
   SetLength(lGDALRowPointer, mImgNumBand);
   for ib:= 0 to mImgNumBand-1 do begin
     lIoTransfer[ib] := CPLMalloc(mLSize[ib] * lWdt * lHgt);
     lGDALRowPointer[ib] := NIL;
   end;

   // Read the pixel data via the GDAL driver an store them in the bitmap
   mLogger.InfoFmt('Load Tile: CR(%d..%d,%d..%d)',[mIX1, mIX2, mIY1, mIY2]) ;

   // Read the band buffer
   for ib:=0 to mImgNumBand-1  do begin
      GDALRasterIO( mLBand[ib], GF_Read,
		    mImgOffsX+lOx, mImgOffsY+lOy, lWdt, lHgt,
		    lIoTransfer[ib], lWdt, lHgt, mLType[ib], 0, 0 );
      lGDALRowPointer[ib] := lIoTransfer[ib];
   end;

   // We iterate in the Pixel address room of GDAL
   // and have to correct the pointer offsets in the BGRA
   for ir:=0 to lHgt-1 do begin
        if ir > oBitmap.Height-1 then break;
        lBGRARowPointer:= oBitmap.ScanLine[ir+lOy];  // Correct Offset Y
        inc(lBGRARowPointer, lOx);                   // Correct Offset X
        for ic:=0 to lWdt-1 do begin
            if  ic > oBitmap.Width then break;
            if lGDALRowPointer[0] <> NIL then begin
                lBGRARowPointer^.red   := lGDALRowPointer[0]^;
                inc(lGDALRowPointer[0])
            end;
            if lGDALRowPointer[1] <> NIL then begin
              lBGRARowPointer^.green := lGDALRowPointer[1]^;
              inc(lGDALRowPointer[1])
            end;
            if lGDALRowPointer[2] <> NIL then begin
              lBGRARowPointer^.blue  := lGDALRowPointer[2]^;
              inc(lGDALRowPointer[2])
            end;
            lBGRARowPointer^.alpha := 255;
            inc(lBGRARowPointer);
        end;
      end;

   for ib:= 0 to mImgNumBand-1 do VSIFree(lIoTransfer[ib]);
   Setlength(lIoTransfer, 0); lIoTransfer := NIL;
   SetLength(lGDALRowPointer,0);       lGDALRowPointer := NIL;
   oBitmap.InvalidateBitmap;
 end;

 function TGdalTifDriver.Init: TResult;
 begin
    result := ResOK;
    if mDriver<> NIL then exit;

    GDALAllRegister;
    // mLogger.InfoLn('RUN: Register all GADL drivers is OK.');

    mDriver := GDALGetDriverByName(GTIFF_FMT);
    if mDriver = NIL then begin
      result := ResError('GDAL image driver '+GTIFF_FMT+' is unknown.');
      exit;
    end else
       mLogger.InfoLn('Load Raster Driver: '+GTIFF_FMT+' OK.');
end;


end.

