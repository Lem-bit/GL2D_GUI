unit dlTextureList;

interface

uses SysUtils, XMLIntf, XMLDoc, Classes, Graphics, dlOpenGL, dlTextureLoader, dlGUITypes;

  //Конвертирование BMP.RGB в BMP.RGBA
  const RGB_RGB  = 1;
  const RGB_RGBA = 2;
  const TListBmpConvert: array[1..2] of TTextureConst =
      (
         (Index: RGB_RGB;  Name: 'RgbRgb'),  //Оставить как есть
         (Index: RGB_RGBA; Name: 'RgbArgb')  //Конвертирование rgb в rgba
      );

  //Формат текстуры
  const TListTexFormat: array[1..4] of TTextureConst =
      (
         (Index: GL_RGB;  Name: 'tfRGB' ),
         (Index: GL_RGBA; Name: 'tfRGBA'),
         (Index: GL_BGR;  Name: 'tfBGR' ),
         (Index: GL_BGRA; Name: 'tfBGRA')
      );

  //Режим отображения
  const TListTexEnvMode: array[1..9] of TTextureConst =
      (
         (Index: GL_REPLACE;     Name: 'emReplace'),
         (Index: GL_DECAL;       Name: 'emDecal'),
         (Index: GL_MODULATE;    Name: 'emModulate'),
         (Index: GL_ADD;         Name: 'emAdd'),
         (Index: GL_ADD_SIGNED;  Name: 'emAddSigned'),
         (Index: GL_INTERPOLATE; Name: 'emInterpolate'),
         (Index: GL_SUBTRACT;    Name: 'emSubtract'),
         (Index: GL_DOT3_RGB;    Name: 'emDot3RGB'),
         (Index: GL_DOT3_RGBA;   Name: 'emDot3RGBA')
      );

  //Фильтр текстуры
  const TListTexFilter: array[1..6] of TTextureConst =
      (
         (Index: GL_NEAREST;                Name: 'Nearest'),
         (Index: GL_LINEAR;                 Name: 'Linear'),
         (Index: GL_NEAREST_MIPMAP_NEAREST; Name: 'NearestMipMapNearest'),
         (Index: GL_NEAREST_MIPMAP_LINEAR;  Name: 'NearestMipMapLinear'),
         (Index: GL_LINEAR_MIPMAP_NEAREST;  Name: 'LinearMipMapNearest'),
         (Index: GL_LINEAR_MIPMAP_LINEAR;   Name: 'LinearMipMapLinear')
      );

type
  TTextureList = class
    private
      FItem  : TList;
      FLoader: TTextureLoader;
    private
      function TexFormatToStr(const AFormat: TUInt): String;
      function StrToTexFormat(const AFormat: String): TUInt;

      function TexEnvModeToStr(const AEnvMode: TUInt): String;
      function StrToTexEnvMode(const AEnvMode: String): TUInt;

      function TexFilterToStr(const AFilter: TUInt): String;
      function StrToTexFilter(const AFilter: String): TUInt;
    private
      function GetTextureById(Id: Integer): TTextureLink;
      function GetTextureByName(AName: String): TTextureLink;
      function GetTextureId(const AName: String): Integer;
      function IndexOf(const id: integer): Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      //Добавить текстуру
      function Add(const AName, AFileName: String): Boolean; overload;
      function Add(const AName, AFileName: String;
         const AFormat: TUInt; const AEnvMode: TUInt; const AFilter: TUInt;
         const ABMPConv: Boolean = false; const ABMPConvAlpha: TColor = clBlack): Boolean; overload;

      //Найти текстуру и вернуть ссылку на текстуру
      function GetLink(const AName: String): Cardinal;
      //Найти текстуру по названию
      function Search(const AName: String): TTextureLink;
      //Удалить текстуру
      function Delete(const AName: String): Boolean;
    public
      //Сохранить список в XML
      function SaveToXML(pFileName: String): Boolean;
      //Загрузить список текстур из XML
      function LoadFromXML(pFileName: String): Boolean;
    public
      property TextureById[id: integer]: TTextureLink  read GetTextureById;
      property Texture[name: string]: TTextureLink read GetTextureByName;
  end;

var TextureList: TTextureList;

implementation

{ TTextureListEx }

function TTextureList.Add(const AName, AFileName: String): Boolean;
begin
  Result:= Add(AFileName, AName,
    TListTexFormat[Low(TListTexFormat)].Index,
    TListTexEnvMode[Low(TListTexEnvMode)].Index,
    TListTexFilter[Low(TListTexFilter)].Index,
    False, clBlack);
end;

function TTextureList.Add(const AName, AFileName: String; const AFormat,
  AEnvMode, AFilter: TUInt;
  const ABMPConv: Boolean = false;
  const ABMPConvAlpha: TColor = clBlack): Boolean;
var Buf: TTextureLink;
begin
  Result:= False;

  //Устанавливаем параметры текстуры
  FLoader.SetTextureOptions(AFormat, AEnvMode, AFilter, ABMPConv, ABMPConvAlpha);

  //Пытаемся загрузить
  Buf:= nil;
  if not FLoader.LoadFromFile(AFileName, Buf) then
    Exit;

  //Если удалось загрузить добавляем в список
  if not Assigned(Buf) then
    Exit;

  Buf.Name  := AName;
  Buf.Enable:= not Buf.IsEmpty;

  FItem.Add(Buf);
  Result:= True;
end;

constructor TTextureList.Create;
begin
  FItem  := TList.Create;
  FLoader:= TTextureLoader.Create;
end;

function TTextureList.Delete(const AName: String): Boolean;
var id : Integer;
begin
  Result:= False;
  try
    id:= GetTextureID(AName);

    if not IndexOf(id) then
      Exit;

    TTextureLink(FItem[id]).Free;
    FItem[id]:= nil;
    FItem.Pack;

  finally
  end;
end;

destructor TTextureList.Destroy;
var i: integer;
begin
  if Assigned(FItem) then
    for i := 0 to FItem.Count - 1 do
      TTextureLink(FItem[i]).Free;

  FreeAndNil(FItem);
  FreeAndNil(FLoader);

  inherited;
end;

function TTextureList.GetLink(const AName: String): Cardinal;
var Buf: TTextureLink;
begin
  Result:= TEX_LINK_EMPTY;
  Buf:= Search(AName);

  if not Assigned(Buf) then
    Exit;

  Result:= Buf.Link;
end;

function TTextureList.GetTextureById(Id: Integer): TTextureLink;
begin
  Result:= nil;
  if IndexOf(id) then
    Result:= TTextureLink(FItem[id]);
end;

function TTextureList.GetTextureByName(AName: String): TTextureLink;
begin
  Result:= Search(AName);
end;

function TTextureList.GetTextureId(const AName: String): Integer;
var Buf: TTextureLink;
begin
  Result:= TEX_LINK_EMPTY;

  Buf:= Search(AName);
  if Assigned(Buf) then
    Result:= Buf.Link;

end;

function TTextureList.IndexOf(const id: integer): Boolean;
begin
  Result:= (Assigned(FItem) and (id > -1) and (id < FItem.Count - 1));
end;

function TTextureList.LoadFromXML(pFileName: String): Boolean;

 function GetAttr(const ANode: IXMLNode; const AAttrName: String; const ADefValue: String): String;
 begin
   Result:= ADefValue;

   if not Assigned(ANode) then
     Exit;

   if not ANode.HasAttribute(AAttrName) then
     Exit;

   Result:= ANode.Attributes[AAttrName];
 end;

 function StrToBool(const AData: String): Boolean;
 begin
   Result:= False;

   if (SameText(AData, 'True')) or
      (SameText(AData, '1'))    then
       Result:= True;
 end;

var XML  : IXMLDocument;
    Root : IXMLNode;
    Node : IXMLNode;
    FID  : Integer;
begin
  Result:= False;

  if not FileExists(pFileName) then
    Exit;

  try
    XML := TXMLDocument.Create(pFileName);
    Root:= XML.ChildNodes.FindNode('TextureList');

    if not Assigned(Root) then
      Exit;

    for FID := 0 to Root.ChildNodes.Count - 1 do
    begin
      Node:= Root.ChildNodes.Nodes[FID];

      Add(
           GetAttr(Node, 'Name', ''),
           GetAttr(Node, 'FileName', ''),
           StrToTexFormat (GetAttr(Node, 'Format',  TListTexFormat[Low(TListTexFormat)].Name)),
           StrToTexEnvMode(GetAttr(Node, 'EnvMode', TListTexEnvMode[Low(TListTexEnvMode)].Name)),
           StrToTexFilter (GetAttr(Node, 'Filter',  TListTexFilter[Low(TListTexFilter)].Name)),
           StrToBool(GetAttr(Node, 'BMPConv', 'False')),
           StrToInt(GetAttr(Node, 'BMPConvAlpha', '0'))
          );
    end;

  finally
  end;
end;

function TTextureList.SaveToXML(pFileName: String): Boolean;
var XML : IXMLDocument;
    Root: IXMLNode;
    Node: IXMLNode;
    i   : Integer;
    Buf : TTextureLink;
begin
  Result:= False;
  if Trim(pFileName) = '' then
    Exit;

  try
    XML:= TXMLDocument.Create(nil);
    XML.Active:= True;
    XML.DocumentElement:= XML.CreateNode('TextureList', ntElement, '');
    Root:= XML.ChildNodes.FindNode('TextureList');

    if not Assigned(Root) then
      Exit;


    for i := 0 to FItem.Count - 1 do
    begin
      Node:= Root.AddChild('Texture');
      Buf := TTextureLink(FItem[i]);

      Node.Attributes['Name']        := Buf.Name;
      Node.Attributes['FileName']    := Buf.FileName;
      Node.Attributes['Format']      := TexFormatToStr(Buf.p_Format);
      Node.Attributes['EnvMode']     := TexEnvModeToStr(Buf.p_EnvMode);
      Node.Attributes['Filter']      := TexFilterToStr(Buf.p_Filter);
      Node.Attributes['BMPConv']     := BoolToStr(Buf.RGBConv);
      Node.Attributes['BMPConvAlpha']:= IntToStr(Buf.RGBMaskColor);
    end;

    XML.SaveToFile(pFileName);
    Result:= True;
  finally

  end;
end;

function TTextureList.Search(const AName: String): TTextureLink;
var i: integer;
begin
  Result:= nil;

  if not Assigned(FItem) then
    Exit;

  for i := 0 to FItem.Count - 1 do
    if SameText(AName, TTextureLink(FItem[i]).Name) then
    begin
      Result:= TTextureLink(FItem[i]);
      Break;
    end;

end;

function TTextureList.StrToTexFormat(const AFormat: String): TUInt;
var i: integer;
begin
  Result:= TListTexFormat[Low(TListTexFormat)].Index;

  for i := Low(TListTexFormat) to High(TListTexFormat) do
    if SameText(AFormat, TListTexFormat[i].Name) then
    begin
      Result:= TListTexFormat[i].Index;
      Break;
    end;

end;

function TTextureList.TexFormatToStr(const AFormat: TUInt): String;
var i: integer;
begin
  Result:= TListTexFormat[Low(TListTexFormat)].Name;

  for i := Low(TListTexFormat) to High(TListTexFormat) do
    if AFormat = TListTexFormat[i].Index then
    begin
      Result:= TListTexFormat[i].Name;
      Break;
    end;

end;

function TTextureList.StrToTexEnvMode(const AEnvMode: String): TUInt;
var i: integer;
begin
  Result:= TListTexEnvMode[Low(TListTexEnvMode)].Index;

  for i := Low(TListTexEnvMode) to High(TListTexEnvMode) do
    if SameText(AEnvMode, TListTexEnvMode[i].Name) then
    begin
      Result:= TListTexEnvMode[i].Index;
      Break;
    end;
end;

function TTextureList.TexEnvModeToStr(const AEnvMode: TUInt): String;
var i: integer;
begin
  Result:= TListTexEnvMode[Low(TListTexEnvMode)].Name;

  for i := Low(TListTexEnvMode) to High(TListTexEnvMode) do
    if AEnvMode = TListTexEnvMode[i].Index then
    begin
      Result:= TListTexEnvMode[i].Name;
      Break;
    end;
end;

function TTextureList.StrToTexFilter(const AFilter: String): TUInt;
var i: integer;
begin
  Result:= TListTexFilter[Low(TListTexFilter)].Index;

  for i := Low(TListTexFilter) to High(TListTexFilter) do
    if SameText(AFilter, TListTexFilter[i].Name) then
    begin
      Result:= TListTexFilter[i].Index;
      Break;
    end;
end;

function TTextureList.TexFilterToStr(const AFilter: TUInt): String;
var i: integer;
begin
  Result:= TListTexFilter[Low(TListTexFilter)].Name;

  for i := Low(TListTexFilter) to High(TListTexFilter) do
    if AFilter = TListTexFilter[i].Index then
    begin
      Result:= TListTexFilter[i].Name;
      Break;
    end;
end;

end.
