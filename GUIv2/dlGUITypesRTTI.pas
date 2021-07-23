unit dlGUITypesRTTI;

interface

uses SysUtils;

{
  ====================================================
  = Delphi OpenGL GUIv2                              =
  =                                                  =
  = Author: Ansperi L.L., 2021                       =
  = Email : gui_proj@mail.ru                         =
  = Site  : lemgl.ru                                 =
  =                                                  =
  ====================================================
}

 type
   TObjRTTIRaw = record
     public
       Raw  : String;
       Data : String;
       Param: String;
     public
       function Head: String;
       function EqualChunk(pLine: String): Boolean;
       function GetValue(pName, pValue: String): String;
       function GetFormat(pObject: TObject): String;
   end;

   TObjRTTIRawType = (rawNull, rawObject, rawEndObject, rawClass, rawEndClass, rawProc, rawEndProc,
      rawItemList, rawEndItemList, rawItem, rawEndItem, rawData);

   TObjRTTIRawData = record
     public
       RawType: TObjRTTIRawType; //Тип записи
       Name   : String;          //Название аттрибута или ноды
       Value  : String;          //Значение аттрибута или ноды
   end;

 const objRTTIRawList: array[TObjRTTIRawType] of TObjRTTIRaw =
          ((Data: ''; Param: ''),
           (Data: 'Object=';      Param: '%s'),
           (Data: 'EndObject=';   Param: '%s'),
           (Data: 'Class=';       Param: '%s'),
           (Data: 'EndClass=';    Param: '%s'),
           (Data: 'Proc=';        Param: '%s'),
           (Data: 'EndProc=';     Param: '%s'),
           (Data: 'ItemList=';    Param: '%s'),
           (Data: 'EndItemList='; Param: '%s'),
           (Data: 'Item=';        Param: '%s'),
           (Data: 'EndItem=';     Param: '%s'),
           (Data: '%s=';          Param: '%s'));

 const objRTTIRaw = '.';

  type
    TObjRTTI = class
      private
      public
        constructor Create;

        //Конвертировать из текста (линии) в значение
        function LineToRawData(pLine: String): TObjRTTIRawData;

        //Определить тип записи
        function GetRawType(pLine: String): TObjRTTIRawType;
        function RawValue(pName, pValue: String): String;
        function RawFormat(pFormatID: TObjRTTIRawType; pObject: TObject): String; overload;
    end;

  var objRTTI: TObjRTTI;

implementation

{ TObjRTTIRaw }

function TObjRTTIRaw.EqualChunk(pLine: String): Boolean;
begin
  Result:= False;
  if Length(pLine) < Length(Head) then Exit;
  Result:= SameText(Head, Copy(pLine, 1, Length(Head)));
end;

function TObjRTTIRaw.GetFormat(pObject: TObject): String;
begin
  Result:= Format(objRTTIRaw + Data + Param, [pObject.ClassName]);
end;

function TObjRTTIRaw.GetValue(pName, pValue: String): String;
begin
  Result:= Format(Data + Param, [pName, pValue]);
end;

function TObjRTTIRaw.Head: String;
begin
  Result:= objRTTIRaw + Data;
end;

{ TObjRTTI }

function TObjRTTI.GetRawType(pLine: String): TObjRTTIRawType;
var i: integer;
begin
  Result:= rawNull;

  for i := integer(Low(TObjRTTIRawType)) + 1 to integer(High(TObjRTTIRawType)) do
    if objRTTIRawList[TObjRTTIRawType(i)].EqualChunk(pLine) then
    begin
      Result:= TObjRTTIRawType(i);
      Break;
    end;

  //Если ни с чем не сопоставили может это данные
  if Result = rawNull then
    if Pos('=', pLine) <> 0 then Result:= rawData;
end;

function TObjRTTI.LineToRawData(pLine: String): TObjRTTIRawData;
var Head   : String;
    HeadLen: Integer;
    Index  : Integer;
begin
  Result.RawType:= rawNull;
  Result.Name   := '';
  Result.Value  := '';

  Result.RawType:= GetRawType(pLine);
  Head   := objRTTIRawList[Result.RawType].Head;
  HeadLen:= Length(Head) + 1;

  if Result.RawType = rawNull then Exit;
  Result.Name:= Head;

  case Result.RawType of
//    rawNull     : ;
    rawObject     ,
    rawEndObject  ,
    rawClass      ,
    rawEndClass   ,
    rawProc       ,
    rawEndProc    ,
    rawItemList   ,
    rawEndItemList,
    rawItem       ,
    rawEndItem    : begin
                      Result.Name := StringReplace(objRTTIRawList[Result.RawType].Data, '=', '', [rfReplaceAll]);
                      Result.Value:= Copy(pLine, HeadLen, Length(pLine) - HeadLen + 1);
                    end;
    rawData       : begin
                      Index:= Pos('=', pLine);
                      if Index < 1 then Exit;

                      Result.Name:= Copy(pLine, 1, Index - 1);
                      Result.Value:= Copy(pLine, Index + 1, Length(pLine) - Index);
                    end;
  end;
end;

function TObjRTTI.RawFormat(pFormatID: TObjRTTIRawType; pObject: TObject): String;
begin
  Result:= objRTTIRawList[pFormatID].GetFormat(pObject);
end;

constructor TObjRTTI.Create;
begin
end;

function TObjRTTI.RawValue(pName, pValue: String): String;
begin
  Result:= objRTTIRawList[rawData].GetValue(pName, pValue);
end;

initialization
  objRTTI:= TObjRTTI.Create;

finalization
  if Assigned(objRTTI) then
    FreeAndNil(objRTTI);

end.
