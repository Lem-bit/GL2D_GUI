unit dlGUIPaletteHelper;

interface

uses SysUtils, dlGUITypes;

{
  ====================================================
  = Delphi OpenGL GUIv2                              =
  =                                                  =
  = Author: Ansperi L.L., 2021                       =
  = Email : gui_proj@mail.ru                         =
  = Site  : lemgl.ru                                 =
  =                                                  =
  = Собрано на Delphi 10.3 community                 =
  ====================================================
}

const DEFAULT_CELL_SIZE  = 18; //Размер ячейки (Квадратный)
      DEFAULT_COUNT_LINE = 7;  //Макс кол-во ячеек в длинну

type
  TGUIPaletteHelper = class
    private
      //Размер ячейки (Квадратный)
      FCellSize    : TInt;
      //Макс кол-во ячеек в длинну
      FCountLineMax: TInt;
    public
      constructor Create(pCellSize: TInt = DEFAULT_CELL_SIZE; pMaxCountInLine: TInt = DEFAULT_COUNT_LINE);
      function GetCellRect(pIndex: TInt): TTextureLinkSquadArr;
  end;

var GUIPalette: TGUIPaletteHelper;

implementation

{ TGUIPalletteHelper }

constructor TGUIPaletteHelper.Create(pCellSize: TInt = DEFAULT_CELL_SIZE; pMaxCountInLine: TInt = DEFAULT_COUNT_LINE);
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

function TGUIPaletteHelper.GetCellRect(pIndex: TInt): TTextureLinkSquadArr;
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
  GUIPalette:= TGUIPaletteHelper.Create();

finalization
  if Assigned(GUIPalette) then
    FreeAndNil(GUIPalette);


end.
