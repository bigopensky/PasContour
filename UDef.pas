unit UDef;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, lgVector, FileUtil;

const
  TKN_STR_NIL  = '*NONE*';
  TKN_NIL      = TKN_STR_NIL;
  TKN_FIRST    = '*FIRST*';
  TKN_LAST     = '*LAST*';
  TKN_DUMMY    = '*';
  TKN_MAGIC    = 'MAGIC';
  EOL          = LineEnding;

  REAL_EPSILON: Double = 4.9406564584124654418e-324;
  REAL_NAN =  0.0/0.0;

  REG_BASE64 = '^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$';

type

  TChar = Char;
  TByte = Byte;

  TString = String; // UnicodeString UTF8String
  TByteString = RawByteString;

  TSize   = NativeUInt;
  TSize8  = Byte;
  TSize16 = Word;
  TSize32 = DWord;
  TSize64 = QWord;


  TUInt8  = TSize8;
  TUInt16 = TSize16;
  TUInt32 = TSize32;
  TUInt64 = TSize64;
  TUInt   = NativeUInt;
  TUInteger = TUInt;

  TInt8    = Int8;
  TInt16   = Int16;
  TInt32   = Int32;
  TInt64   = Int64;
  TInt     = NativeInt;
  TInteger = TInt;

  TMillies = Comp;

  TBoolean = boolean;
  TIntegerList = specialize TGVector<Integer>;

  TReal32  = Single;
  TReal    = Double;
  TReal64  = Double;
  TReal80  = Extended;

  TBytes32 = array [0..3] of TUInt8;
  TBytes64 = array [0..7] of TUInt8;
  TBytes   = array of TUInt8;
  TReal32Vector = array of TReal32;

  TReal32Matrix = array of array of TReal32;
  TRealMatrix  =  array of array of TReal;
  TAttrMatrix  = array of array of TSize16;
  TByteMatrix  = array of array of TSize8;

  TAffineTransform = array [1..2, 1..3] of TReal;
  TOrthoTransform  = array [1..3, 1..3] of TReal;


TResult = record
  Code:    TInteger;
  Message:  TString;
  Error:    TString;
  Details:  TString;
  FName:   TString;
  Line:     TInteger;
  Ok:       TBoolean;
end;

TFatalRuntimeException = Class(Exception);

function ResOK(const aMsg: TString = ''): TResult;
function ResFatal(const aMsg: TString): TResult;
function ResError(const aMsg: TString;
                  const aDetail: TString = '';
                        aCode: TInteger = 1): TResult;


implementation

function ResOK(const aMsg: TString = ''): TResult;
 begin
   with result do begin
       Code    := 0;
       Message := aMsg;
       Error   := '';
       Details := '';
       Fname   := '';
       Line    := 0;
       ok      := true;
   end;
 end;

 function ResError(const aMsg: TString;
                   const aDetail: TString;
                         aCode: TInteger): TResult;
 begin
   with result do begin
       error   := aMsg;
       details := aDetail;
       code    := aCode;
       fname   := '';
       line    := 0;
       ok      := false;
   end;
 end;

 function ResFatal(const aMsg: TString): TResult;
 begin
     raise TFatalRuntimeException.Create('FATAL.RUNTIME:ERROR:'+ aMsg);
 end;




end.

