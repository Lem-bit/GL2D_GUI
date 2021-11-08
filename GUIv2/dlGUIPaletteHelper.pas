unit dlGUIPaletteHelper;

interface

uses SysUtils, dlGUITypes;

{
  ====================================================
  = Delphi OpenGL GUIv2                              =
  =                                                  =
  = Author  : Ansperi L.L., 2021                     =
  = Email   : gui_proj@mail.ru                       =
  = Site    : lemgl.ru                               =
  = Telegram: https://t.me/delphi_lemgl              =
  =                                                  =
  ====================================================
}

const DEFAULT_CELL_SIZE  = 18; //Размер ячейки (Квадратный)
      DEFAULT_COUNT_LINE = 7;  //Макс кол-во ячеек в длинну

const
   pal_0      = 0;
   pal_1      = 1;
   pal_2      = 2;
   pal_3      = 3;
   pal_Window = 4;
   pal_Frame  = 5;
   pal_6      = 6;
   pal_7      = 7;

   pal_CheckBox_uc    = 8;
   pal_CheckBox_ch    = 9;
   pal_RadioButton_uc = 10;
   pal_RadioButton_ch = 11;
   pal_ArrowUp        = 12;
   pal_ArrowDn        = 13;
   pal_ArrowLeft      = 14;
   pal_ArrowRight     = 15;

   pal_16       = 16;
   pal_17       = 17;
   pal_Mouse    = 18;
   pal_19       = 19;
   pal_20       = 20;
   pal_21       = 21;
   pal_22       = 22;
   pal_PopupDiv = 23;

   pal_Track         = 24;
   pal_TrackerVert   = 25;
   pal_TrackerHoriz  = 26;
   pal_BtnCloseUp    = 27;
   pal_BtnCloseDn    = 28;
   pal_BtnMinimizeUp = 29;
   pal_BtnMinimizeDn = 30;
   pal_31            = 31;

   pal_32 = 32;
   pal_33 = 33;
   pal_34 = 34;
   pal_35 = 35;
   pal_36 = 36;
   pal_37 = 37;
   pal_38 = 38;
   pal_Transparent = 39;

type
  TGUIPaletteHelper = class
    strict private
      //Размер ячейки (Квадратный)
      FCellSize    : Integer;
      //Макс кол-во ячеек в длинну
      FCountLineMax: Integer;
    public
      constructor Create(pCellSize: Integer = DEFAULT_CELL_SIZE; pMaxCountInLine: Integer = DEFAULT_COUNT_LINE);
      function GetCellRect(pIndex: Integer): TTextureLinkSquadArr;
  end;

var GUIPalette: TGUIPaletteHelper;

implementation

{ TGUIPalletteHelper }

constructor TGUIPaletteHelper.Create(pCellSize: Integer = DEFAULT_CELL_SIZE; pMaxCountInLine: Integer = DEFAULT_COUNT_LINE);
begin

  if pCellSize < 0 then
    FCellSize:= DEFAULT_CELL_SIZE
  else
    FCellSize:= pCellSize;

  if pMaxCountInLine < 0 then
    FCountLineMax:= DEFAULT_COUNT_LINE
  else
    FCountLineMax:= pMaxCountInLine;

end;

function TGUIPaletteHelper.GetCellRect(pIndex: Integer): TTextureLinkSquadArr;
var X: Integer;
    Y: Integer;

    Width: Integer;
begin
  //Общая ширина
  Width:= FCellSize * (FCountLineMax + 1);

  //Координаты по индексу
  X:= ((pIndex) * FCellSize) mod Width;
  Y:= (((pIndex) * FCellSize) div Width) * FCellSize;

  //Левая верхняя точка на текстуре
  Result.Index[0].U:= X + 1;
  Result.Index[0].V:= Y + 1;

  //Правая верхняя точка на текстуре
  Result.Index[1].U:= X + FCellSize - 1;
  Result.Index[1].V:= Y + 1;

  //Правая нижняя точка на текстуре
  Result.Index[2].U:= X + FCellSize - 1;
  Result.Index[2].V:= Y + FCellSize - 1;

  //Левая нижняя точка на текстуре
  Result.Index[3].U:= X + 1;
  Result.Index[3].V:= Y + FCellSize - 1;
end;

initialization
  if not Assigned(GUIPalette) then
    GUIPalette:= TGUIPaletteHelper.Create();

finalization
  if Assigned(GUIPalette) then
    FreeAndNil(GUIPalette);


end.
