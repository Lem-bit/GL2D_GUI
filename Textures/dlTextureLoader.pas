unit dlTextureLoader;

interface

uses Windows, JPEG, SysUtils, Classes, Graphics, dlGUITypes, dlOpenGL;

type
  TGAHeader = packed record   // Header type for TGA images
    FileType     : Byte;
    ColorMapType : Byte;
    ImageType    : Byte;
    ColorMapSpec : Array[0..4] of Byte;
    OrigX  : Array [0..1] of Byte;
    OrigY  : Array [0..1] of Byte;
    Width  : Array [0..1] of Byte;
    Height : Array [0..1] of Byte;
    BPP    : Byte;
    ImageInfo : Byte;
  end;

  //
  TTextureConst = record
    Index: TUInt;  //OPENGL имя
    Name : String; //XML имя
  end;

type
  //Тип файла загрузки
  TTextureFileType = (F_UNKNOWN, F_BMP, F_JPG, F_TGA);

  //Пустая текстура
  const FREE_LINK = 0;

type
  //Информация о загруженной текстуре для передачи в FTexture
  TTextureInfo = record
    public
      Width : Integer;
      Height: Integer;
      Link  : TUInt;
      Load  : Boolean;
    public
      procedure Free;
  end;

  //Загрузчик текстуры
  TTextureConverter = record
    public
      Enable    : Boolean;  //Включен конвертер
      AlphaColor: TGLColor; //Альфа цвет
  end;

  TTextureLoader = class
    private
      FTConverter      : TTextureConverter; //Конвертер

      FTFormat         : TUInt;    //Формат текстуры
      FTEnvMode        : TUInt;    //Режим отображения текстуры
      FTFilter         : TUInt;    //Фильтр текстуры

      FErrors          : TTextureError; //Ошибки загрузки
      FLoadFromResource: Boolean; //Загрузка с ресурсов
    private
      function CreateTexture(const AWidth, AHeight: Integer; const AData: Pointer): TUInt;

      function LoadBMPFromFile(const AFileName: String; out InfoHeader: BITMAPINFOHEADER): Pointer;
      function LoadBMPFromRes(const AName: String; out InfoHeader: BITMAPINFOHEADER): Pointer;
      function LoadBMP (const AFileName: String): TTextureInfo;

      function LoadJPGFromRes(const AName: String; var JPG: TJPEGImage): Boolean;
      function LoadJPG(const AFileName: String): TTextureInfo;

      function LoadTGAFromFile(const AFileName: String; out InfoHeader: TGAHeader): Pointer;
      function LoadTGAFromRes(const AName: String; out InfoHeader: TGAHeader): Pointer;
      function LoadTGA (const AFileName: String): TTextureInfo;
    private
      //Определить тип загружаемого файла
      function CaseFileType(const AFileName: String): TTextureFileType;
      //
      function LoadTexture(const AFileName: String; var ATextureLink: TTextureLink): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
    public
      //Установить параметры загружаемой текстуры
      procedure SetTextureOptions(const AFormat: TUInt; const AEnvMode: TUInt; const AFilter: TUInt;
         const ABMPConv: Boolean = false; const ABMPConvAlpha: TColor = clBlack);

      function LoadFromFile(const AFileName: String; var ATextureLink: TTextureLink): Boolean;
      function LoadFromResource(const AFileName: String; var ATextureLink: TTextureLink): Boolean;
    public
      property Errors : TTextureError  read FErrors;
  end;

implementation

{ TTextureLoader }

function TTextureLoader.CaseFileType(const AFileName: String): TTextureFileType;
var i, num: integer;
    buf   : string;
begin
  Result:= F_UNKNOWN;

  for i := Length(AFileName) downto 1 do
    if AFileName[i] = '.' then
    begin
      num:= Length(AFileName) - i;
      Buf:= AnsiLowerCase(Copy(AFileName, Length(AFileName) - num, num + 1));
      Break;
    end;

  if SameText(Buf, '.bmp') then
    Result:= F_BMP
  else if SameText(Buf, '.jpg') or SameText(Buf, '.jpeg') then
    Result:= F_JPG
  else if SameText(Buf, '.tga') then
    Result:= F_TGA;
end;

constructor TTextureLoader.Create;
begin
  FTConverter.Enable    := False;
  FTConverter.AlphaColor:= TGLColor.Create(clBlack);
  FTFormat              := GL_RGB;
  FTEnvMode             := GL_MODULATE;
  FTFilter              := GL_LINEAR;
  FLoadFromResource     := False;
end;

function TTextureLoader.CreateTexture(const AWidth, AHeight: Integer; const AData: Pointer): TUInt;
var Texture: TUInt;
begin
  Result:= FREE_LINK;
  try
     glGenTextures(1, @Texture);
     glBindTexture(GL_TEXTURE_2D, Texture);

     glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, FTEnvMode);

     case FTFilter of
       GL_NEAREST,
       GL_LINEAR :
         begin
           glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
           glTexImage2D(GL_TEXTURE_2D, 0, FTFormat, AWidth, AHeight, 0, FTFormat, GL_UNSIGNED_BYTE, AData);
         end;

       GL_NEAREST_MIPMAP_NEAREST,
       GL_LINEAR_MIPMAP_NEAREST ,
       GL_NEAREST_MIPMAP_LINEAR ,
       GL_LINEAR_MIPMAP_LINEAR  :
         begin
           gluBuild2DMipmaps(GL_TEXTURE_2D, 4, AWidth, AHeight, FTFormat, GL_UNSIGNED_BYTE, AData);
         end;
     end;

     glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, FTFilter);
     //Nearest or Linear
     glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, FTFilter);

     Result:= Texture;
  except
  end;
end;

destructor TTextureLoader.Destroy;
begin
  FreeAndNil(FTConverter.AlphaColor);
  inherited;
end;

function TTextureLoader.LoadFromFile(const AFileName: String; var ATextureLink: TTextureLink): Boolean;
begin
  FLoadFromResource:= False;
  Result:= LoadTexture(AFileName, ATextureLink);
end;

function TTextureLoader.LoadFromResource(const AFileName: String; var ATextureLink: TTextureLink): Boolean;
begin
  FLoadFromResource:= True;
  Result:= LoadFromFile(AFileName, ATextureLink);
end;

function TTextureLoader.LoadBMP(const AFileName: String): TTextureInfo;
type
  TPixelRGB = record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  TPixelRGBA = record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

var Data      : Pointer;
    InfoHeader: BITMAPINFOHEADER;
    ADataRGB  : array of TPixelRGB;
    ADataRGBA : array of TPixelRGBA;
    i         : integer;
    BitmapLen : integer;
begin
  Result.Free;
  Data:= nil;

  try
    //Загрузка данных
    if FLoadFromResource then
      //С ресурсов
      Data:= LoadBMPFromRes(AFileName, InfoHeader)
    else
      //Из файла
      Data:= LoadBMPFromFile(AFileName, InfoHeader);

    if Data = nil then
    begin
      Errors.SetError(True, Format('При загрузке файла "%s" возникли ошибки.', [AFileName]));
      Exit;
    end;

    BitmapLen:= (InfoHeader.biWidth * InfoHeader.biHeight * InfoHeader.biBitCount Div 8);

    case FTFormat of
      GL_RGB, GL_BGR:
      begin

        if (FTConverter.Enable) and (FTFormat = GL_RGB) then
        begin
          //Читаем RGB, меняем местами B<=>R, превращаем в RGBA
          SetLength(ADataRGBA, BitmapLen div 3);

          for i := 0 to InfoHeader.biWidth * InfoHeader.biHeight - 1 do
          begin
            ADataRGBA[i].B:= Byte(Pointer(Integer(Data) + I * 3)^);
            ADataRGBA[i].G:= Byte(Pointer(Integer(Data) + I * 3 + 1)^);
            ADataRGBA[i].R:= Byte(Pointer(Integer(Data) + I * 3 + 2)^);

            if FTConverter.AlphaColor.GetColor = Integer(RGB(ADataRGBA[i].R, ADataRGBA[i].G, ADataRGBA[i].B)) then
              ADataRGBA[i].A:= 0
            else
              ADataRGBA[i].A:= 255;
          end;

          FTFormat:= GL_RGBA;
          Result.Link:= CreateTexture(InfoHeader.biWidth, InfoHeader.biHeight, ADataRGBA);
        end
        else
        begin
          //Читаем RGB и меняем местами B<=>R
          SetLength(ADataRGB, BitmapLen div 3);

          for i := 0 to InfoHeader.biWidth * InfoHeader.biHeight - 1 do
          begin
            ADataRGB[i].B:= Byte(Pointer(Integer(Data) + I * 3)^);
            ADataRGB[i].G:= Byte(Pointer(Integer(Data) + I * 3 + 1)^);
            ADataRGB[i].R:= Byte(Pointer(Integer(Data) + I * 3 + 2)^);
          end;

          Result.Link:= CreateTexture(InfoHeader.biWidth, InfoHeader.biHeight, ADataRGB);
        end;
      end;

      GL_RGBA, GL_BGRA:
      begin
        //Читаем RGBA и меняем местами B<=>R
        SetLength(ADataRGBA, BitmapLen div 3);

        for i := 0 to InfoHeader.biWidth * InfoHeader.biHeight - 1 do
        begin
          ADataRGBA[i].B:= Byte(Pointer(Integer(Data) + I * 4)^);
          ADataRGBA[i].G:= Byte(Pointer(Integer(Data) + I * 4 + 1)^);
          ADataRGBA[i].R:= Byte(Pointer(Integer(Data) + I * 4 + 2)^);
          ADataRGBA[i].A:= Byte(Pointer(Integer(Data) + I * 4 + 3)^)
        end;

        Result.Link:= CreateTexture(InfoHeader.biWidth, InfoHeader.biHeight, ADataRGBA)
      end;

    end;

    Result.Width := InfoHeader.biWidth;
    Result.Height:= InfoHeader.biHeight;
    Result.Load  := True;

  finally
    ADataRGB := nil;
    ADataRGBA:= nil;

    if Data <> nil then
     FreeMem(Data);
  end;

end;

function TTextureLoader.LoadBMPFromFile(const AFileName: String; out InfoHeader: BITMAPINFOHEADER): Pointer;
var BitmapFile   : THandle;
    FileHeader   : BITMAPFILEHEADER;
    ReadBytes    : LongWord;
    BitmapLength : LongWord;
    PaletteLength: LongWord;
    Palette      : array of RGBQUAD;
begin
  Result:= nil;

  try
    // Load image from file
    BitmapFile := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);

    if (BitmapFile = INVALID_HANDLE_VALUE) then
    begin
      Errors.SetError(True, Format('Ошибка загрузки данных из файла "%s"', [AFileName]));
      Exit;
    end;

    try
      // Get header information
      ReadFile(BitmapFile, FileHeader, SizeOf(FileHeader), ReadBytes, nil);
      ReadFile(BitmapFile, InfoHeader, SizeOf(InfoHeader), ReadBytes, nil);

      // Get palette
      PaletteLength := InfoHeader.biClrUsed;
      SetLength(Palette, PaletteLength);
      ReadFile(BitmapFile, Palette, PaletteLength, ReadBytes, nil);

      if (ReadBytes <> PaletteLength) then
      begin
        Errors.SetError(True, Format('Ошибка чтения палитры BMP из файла "%s"', [AFileName]));
        Exit;
      end;

      BitmapLength := InfoHeader.biSizeImage;
      if BitmapLength = 0 then
        BitmapLength := InfoHeader.biWidth * InfoHeader.biHeight * InfoHeader.biBitCount Div 8;

      // Get the actual pixel data
      GetMem(Result, BitmapLength);
      ReadFile(BitmapFile, Result^, BitmapLength, ReadBytes, nil);

      if (ReadBytes <> BitmapLength) then
      begin
        Errors.SetError(True, Format('Ошибка чтения BTIMAP данных из "%s"', [AFileName]));
        Exit;
      end;

    finally
      CloseHandle(BitmapFile);
    end;
  except
    on e: exception do
    begin
      Result:= nil;
      Errors.SetError(True, Format('При чтении файла "%s" возникала ошибка %s', [AFileName, e.Message]));
    end;
  end;
end;

function TTextureLoader.LoadBMPFromRes(const AName: String; out InfoHeader: BITMAPINFOHEADER): Pointer;
var ResHandle    : THandle;
    MemHandle    : THandle;
    ResPtr       : PByte;
    ResSize      : Longint;
    MemStream    : TMemoryStream;
    FileHeader   : BITMAPFILEHEADER;
    PaletteLength: LongWord;
    Palette      : array of RGBQUAD;
    BitmapLength : LongWord;

begin
  Result:= nil;

  try
    ResHandle := FindResource(hInstance, PChar(copy(AName, 1, Pos('.', AName)-1)), 'BMP');
    if ResHandle = 0 then
    begin
      Errors.SetError(True, Format('Ошибка загрузки "%s" из ресурсов.', [AName]));
      Exit;
    end;

    try
      MemHandle := LoadResource(hInstance, ResHandle);
      ResPtr    := LockResource(MemHandle);
      MemStream := TMemoryStream.Create;
      ResSize := SizeOfResource(hInstance, ResHandle);
      MemStream.SetSize(ResSize);
      MemStream.Write(ResPtr^, ResSize);
      FreeResource(MemHandle);
      MemStream.Seek(0, 0);

      MemStream.ReadBuffer(FileHeader, SizeOf(FileHeader));  // FileHeader
      MemStream.ReadBuffer(InfoHeader, SizeOf(InfoHeader));  // InfoHeader
      PaletteLength := InfoHeader.biClrUsed;
      SetLength(Palette, PaletteLength);
      MemStream.ReadBuffer(Palette, PaletteLength); // Palette

      BitmapLength := InfoHeader.biSizeImage;
      if BitmapLength = 0 then
        BitmapLength := InfoHeader.biWidth * InfoHeader.biHeight * InfoHeader.biBitCount Div 8;

      GetMem(Result, BitmapLength);
      MemStream.ReadBuffer(Result^, BitmapLength); // Bitmap Data
    finally
      FreeAndNil(MemStream);
    end;
  except
    on e: exception do
    begin
      Result:= nil;
      Errors.SetError(True, Format('При загрузке данных "%s" с ресурсов возникла ошибка: %s', [AName, e.Message]));
    end;
  end;
end;

function TTextureLoader.LoadJPG(const AFileName: String): TTextureInfo;
var JPG : TJPEGImage;
    Load: Boolean;
    BMP : TBitmap;
    Data: Array of LongWord;
    W   : Integer;
    H   : Integer;
    Line: ^LongWord;
    C   : LongWord;
begin
  Result.Free;
  Data:= nil;
  Load:= False;

  try
    JPG:= TJPEGImage.Create;

    if FLoadFromResource then
      Load:= LoadJPGFromRes(AFileName, JPG)
    else
    try
      if FileExists(AFileName) then
      begin
        JPG.LoadFromFile(AFileName);
        Load:= True;
      end
      else
        raise Exception.Create(Format('Не удалось найти файл "%s" ', [AFileName]));
    except
      on e: exception do
        Errors.SetError(True, E.Message);
    end;

    if not Load then
      Exit;

    BMP            := TBitmap.Create;
    BMP.AlphaFormat:= afIgnored;
    BMP.PixelFormat:= pf32bit;
    BMP.Width      := JPG.Width;
    BMP.Height     := JPG.Height;
    BMP.Canvas.Draw(0, 0, JPG);

    SetLength(Data, BMP.Width * BMP.Height);

    For H:= 0 to BMP.Height - 1 do
    begin
      Line:= BMP.ScanLine[BMP.Height - H - 1];   // flip JPEG

      For W:= 0 to BMP.Width - 1 do
      Begin
        C:= Line^ and $FFFFFFFF; // Need to do a color swap
        Data[W + (H * BMP.Width)] :=
          (((c and $FF) shl 16) + (c shr 16) + (c and $FF00)) or $FF000000;  // 4 channel.
        inc(Line);
      end;
    end;

    Result.Width := BMP.Width;
    Result.Height:= BMP.Height;
    Result.Link  := CreateTexture(BMP.Width, BMP.Height, Data);
    Result.Load  := True;

  finally
    FreeAndNil(BMP);
    FreeAndNil(JPG);
    Data:= nil;
  end;

end;

function TTextureLoader.LoadJPGFromRes(const AName: String; var JPG: TJPEGImage): Boolean;
var ResHandle: THandle;
    MemHandle: THandle;
    ResPtr   : PByte;
    MemStream: TMemoryStream;
    ResSize  : Longint;
begin
  Result:= False;
  if not Assigned(JPG) then
    Exit;

  try
    ResHandle := FindResource(hInstance, PChar(copy(AName, 1, Pos('.', AName)-1)), 'JPEG');
    if ResHandle = 0 then
      ResHandle := FindResource(hInstance, PChar(copy(AName, 1, Pos('.', AName)-1)), 'JPG');

    if ResHandle = 0 then
    begin
      Errors.SetError(True, Format('Ошибка загрузки "%s" из ресурсов.', [AName]));
      Exit;
    end;

    MemHandle := LoadResource(hInstance, ResHandle);
    try
      ResPtr    := LockResource(MemHandle);
      ResSize   := SizeOfResource(hInstance, ResHandle);
      MemStream := TMemoryStream.Create;

      MemStream.SetSize(ResSize);
      MemStream.Write(ResPtr^, ResSize);
      MemStream.Seek(0, 0);

      JPG.LoadFromStream(MemStream);
      Result:= True;
    finally
      FreeResource(MemHandle);
      FreeAndNil(MemStream);
    end;
  except
    on e: exception do
      Errors.SetError(True, Format('При загрузке данных "%s" с ресурсов возникла ошибка: %s', [AName, e.Message]));
  end;
end;

function TTextureLoader.LoadTexture(const AFileName: String; var ATextureLink: TTextureLink): Boolean;
var TextureInfo: TTextureInfo;
begin
  Result:= False;

  try
    if not FileExists(AFileName) then
    begin
      FErrors.SetError(True, Format('Файл: "%s" не найден.', [AFileName]));
      Exit;
    end;

    case CaseFileType(AFileName) of
      F_UNKNOWN: begin
        Errors.SetError(True, Format('Формат файла: "%s" не поддерживается.', [AFileName]));
        Exit;
      end;

      F_BMP: TextureInfo:= LoadBMP(AFileName);
      F_JPG: TextureInfo:= LoadJPG(AFileName);
      F_TGA: TextureInfo:= LoadTGA(AFileName);
    end;

    if not TextureInfo.Load then
      Exit;

    //Заполнение данных о текстуре
    if not Assigned(ATextureLink) then
      ATextureLink:= TTextureLink.Create;

    ATextureLink.Width       := TextureInfo.Width;
    ATextureLink.Height      := TextureInfo.Height;
    ATextureLink.Link        := TextureInfo.Link;
    ATextureLink.FileName    := AFileName;
    ATextureLink.p_Format    := FTFormat;
    ATextureLink.p_EnvMode   := FTEnvMode;
    ATextureLink.p_Filter    := FTFilter;
    ATextureLink.RGBConv     := FTConverter.Enable;
    ATextureLink.RGBMaskColor:= FTConverter.AlphaColor.GetColor;

    Result:= True;
  except

  end;
end;

function TTextureLoader.LoadTGA(const AFileName: String): TTextureInfo;
var Data      : Pointer;
    InfoHeader: TGAHeader;
begin
  Result.Free;
  Data:= nil;

  try
    //Загрузка данных
    if FLoadFromResource then
      //С ресурсов
      Data:= LoadTGAFromRes(AFileName, InfoHeader)
    else
      //Из файла
      Data:= LoadTGAFromFile(AFileName, InfoHeader);

    if Data = nil then
    begin
      Errors.SetError(True, Format('При загрузке файла "%s" возникли ошибки.', [AFileName]));
      Exit;
    end;

    Result.Width := InfoHeader.Width[0]  + InfoHeader.Width[1]  * 256;
    Result.Height:= InfoHeader.Height[0] + InfoHeader.Height[1] * 256;
    Result.Link  := CreateTexture(Result.Width, Result.Height, Data);
    Result.Load  := True;

  finally
    if Data <> nil then
     FreeMem(Data);
  end;

(*
  // TGAs are stored BGR and not RGB, so swap the R and B bytes.
  // 32 bit TGA files have alpha channel and gets loaded differently
  if TGAHeader.BPP = 24 then
  begin
    for I := 0 to Result.Width * Result.Height - 1 do
    begin
      Front := Pointer(Integer(Image) + I*3);
      Back  := Pointer(Integer(Image) + I*3 + 2);
      Temp  := Front^;
      Front^:= Back^;
      Back^ := Temp;
    end;

    TexFormat     := GL_RGB;
    Result.Link   := CreateTexture(Image, Result.Width, Result.Height);
    Result.GLError:= glGetError;

  end
  else
  begin
    for I :=0 to Result.Width * Result.Height - 1 do
    begin
      Front := Pointer(Integer(Image) + I * 4);
      Back  := Pointer(Integer(Image) + I * 4 + 2);
      Temp  := Front^;
      Front^:= Back^;
      Back^ := Temp;
    end;

    TexFormat     := GL_RGBA;
    Result.Link   := CreateTexture(Image, Result.Width, Result.Height);
    Result.GLError:= glGetError;
  end;

  FreeMem(Image); *)
end;

function TTextureLoader.LoadTGAFromFile(const AFileName: String; out InfoHeader: TGAHeader): Pointer;
var TGAFile  : THandle;
    ReadBytes: LongWord;
    ImageSize: LongWord;
    Width    : Integer;
    Height   : Integer;
begin
  Result:= nil;

  if not FileExists(AFileName) then
  begin
    FErrors.SetError(True, Format('Файл: "%s" не найден.', [AFileName]));
    Exit;
  end;

  try
    // Load image from file
    TGAFile := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);

    if (TGAFile = INVALID_HANDLE_VALUE) then
    begin
      Errors.SetError(True, Format('Ошибка загрузки данных из файла "%s"', [AFileName]));
      Exit;
    end;

    try
      // Get header information
      ReadFile(TGAFile, InfoHeader, SizeOf(InfoHeader), ReadBytes, nil);

      Width     := InfoHeader.Width[0]  + InfoHeader.Width[1]  * 256;
      Height    := InfoHeader.Height[0] + InfoHeader.Height[1] * 256;
      ImageSize := Width * Height * (InfoHeader.BPP div 8);

      if (InfoHeader.ImageType <> 2) then  { TGA_RGB }
      begin
        Errors.SetError(True, 'Поддерживаются только несжатые изображения ');
        Exit;
      end;

      // Don't support colormapped files
      if InfoHeader.ColorMapType <> 0 then
      begin
        Errors.SetError(True, 'Не поддерживаются файлы с цветовым отображением');
        Exit;
      end;

      if InfoHeader.BPP < 24 then
      begin
        Errors.SetError(True, 'Не поддерживаются файлы с цветом меньше 24 бит');
        Exit;
      end;

      // Get the actual pixel data
      GetMem(Result, ImageSize);
      ReadFile(TGAFile, Result^, ImageSize, ReadBytes, nil);

      if ReadBytes <> ImageSize then
        FreeMem(Result);

    finally
      CloseHandle(TGAFile);
    end;
  except
    on e: exception do
    begin
      Result:= nil;
      Errors.SetError(True, Format('При чтении файла "%s" возникала ошибка %s', [AFileName, e.Message]));
    end;
  end;

end;

function TTextureLoader.LoadTGAFromRes(const AName: String; out InfoHeader: TGAHeader): Pointer;
var ResStream: TResourceStream;
    ImageSize: LongWord;
    Width    : Integer;
    Height   : Integer;
begin
  Result:= nil;

  try
    ResStream := TResourceStream.Create(hInstance, PChar(copy(AName, 1, Pos('.', AName) -1)), 'TGA');

    try
      ResStream.ReadBuffer(InfoHeader, SizeOf(TGAHeader));  // FileHeader

      // Get the width, height, and color depth
      Width     := InfoHeader.Width[0]  + InfoHeader.Width[1]  * 256;
      Height    := InfoHeader.Height[0] + InfoHeader.Height[1] * 256;
      ImageSize := Width * Height * (InfoHeader.BPP div 8);

      if (InfoHeader.ImageType <> 2) then  { TGA_RGB }
      begin
        Errors.SetError(True, 'Поддерживаются только несжатые изображения ');
        Exit;
      end;

      // Don't support colormapped files
      if InfoHeader.ColorMapType <> 0 then
      begin
        Errors.SetError(True, 'Не поддерживаются файлы с цветовым отображением');
        Exit;
      end;

      if InfoHeader.BPP < 24 then
      begin
        Errors.SetError(True, 'Не поддерживаются файлы с цветом меньше 24 бит');
        Exit;
      end;

      GetMem(Result, ImageSize);
      ResStream.ReadBuffer(Result^, ImageSize);
    finally
      ResStream.Free;
    end;

  except
    on e: exception do
    begin
      Result:= nil;
      Errors.SetError(True, Format('При загрузке данных "%s" с ресурсов возникла ошибка: %s', [AName, e.Message]));
    end;
  end;

end;

procedure TTextureLoader.SetTextureOptions(const AFormat, AEnvMode, AFilter: TUInt;
   const ABMPConv: Boolean = false; const ABMPConvAlpha: TColor = clBlack);
begin
  FTConverter.Enable:= ABMPConv;
  FTConverter.AlphaColor.SetColor(ABMPConvAlpha);
  FTFormat := AFormat;
  FTEnvMode:= AEnvMode;
  FTFilter := AFilter;
end;

{ TTextureInfo }

procedure TTextureInfo.Free;
begin
  Width := 0;
  Height:= 0;
  Link  := 0;
  Load  := False;
end;

end.
