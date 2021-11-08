unit dlGUIFormList;

interface

  uses Messages, Classes, Controls, Windows, SysUtils, dlGUITypes, dlGUIObject, dlGUIForm,
   dlGUIMainMenu;

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

  type
   //Список окон
   TGUIFormList = class
     strict private
       //Ссылка на текущую форму
       FCurrentForm : Pointer;
       //Список форм
       FFormList    : TList;
       FLastMousePos: TPoint;

       FMouseDown   : Boolean;
       FMainMenu    : TGUIMainMenu;
     strict private
       function IsExists(pNum: integer): Boolean;
       procedure CurrentFormDeactivate;
       function CurrentFormAccessible: Boolean;
       function ConvertMouseButton(MouseButton: TMouseButton): TGUIMouseButton;
     public
       function Count: Integer;
     public
       //Создать класс списка окон
       constructor Create;

       //Создать окно
       procedure AddForm(pForm: TGUIForm);
       procedure CreateForm(pName, pCaption: String; pX, pY, pWidth, pHeight: Integer; pTextureLink: TTextureLink = nil; pTextureFont: TTextureLink = nil);

       //Прорисовать все окна
       procedure Render;

       //Активировать окно
       procedure SetActive(pForm: TGUIForm); overload;
       procedure SetActive(pFormName: String); overload;
       procedure SetActive(pFormID: Integer); overload;

       //Получить ссылку на текущее окно (выбранное, выделенное)
       function GetCurrentWindow: TGUIForm;
       function GetForm(id: integer): TGUIForm; overload;
       function GetForm(pName: string): TGUIForm; overload;

       //Нажатие кнопки на клавиатуре
       procedure OnKeyDown(var Key: Word; Shift: TShiftState); virtual;
       //Отпускание кнопки на клавиатуре
       procedure OnKeyUp(var Key: Word; Shift: TShiftState); virtual;
       //Определение кнопки
       procedure OnKeyPress(Key: Char); virtual;

       //Прокрутка колесика мыши вверх
       function OnMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
       //Прокрутка колесика мыши вниз
       function OnMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
       //Нажатие кнопки мыши
       function OnMouseDown(pX, pY: Integer; Button: TMouseButton): Boolean;
       //Отпускание кнопки мыши
       function OnMouseUp(pX, pY: Integer; Button: TMouseButton): Boolean;
       //Событие двойное нажатие мыши (даблклик)
       function OnMouseDoubleClick(pX, pY: Integer; Button: TMouseButton): Boolean;
       //Перемещение мыши
       function OnMouseMove(pX, pY: Integer): Boolean;
       //Ищем окно под этими координатами
       function OnHit(pX, pY: Integer): Boolean;
       //Удалить все формы
       destructor Destroy; override;
       //Закрыть форму
       function CloseForm(index: integer): Boolean;
       //Удалить ссылки на nil в списке форм
       procedure ClearNullReference;

       function WndProc(var AMessage: TMessage): Boolean;
     public
       property MainMenu: TGUIMainMenu         read FMainMenu write FMainMenu;
       property Form[index: integer]: TGUIForm read GetForm;
   end;

var FormList: TGUIFormList;

implementation

{ TFormGUIList }

constructor TGUIFormList.Create;
begin
  FFormList   := TList.Create;
  FCurrentForm:= nil;
end;

procedure TGUIFormList.CreateForm(pName, pCaption: String; pX, pY, pWidth, pHeight: Integer; pTextureLink: TTextureLink = nil; pTextureFont: TTextureLink = nil);
begin
  AddForm(TGUIForm.Create(pName, pCaption, pX, pY, pWidth, pHeight, pTextureLink, pTextureFont));
end;

procedure TGUIFormList.AddForm(pForm: TGUIForm);
begin
  if not Assigned(pForm) then
    Exit;

  pForm.ID:= FFormList.Count + 1;
  FFormList.Add(pForm);
  FCurrentForm:= nil;
end;

function TGUIFormList.CurrentFormAccessible: Boolean;
begin
  Result:= (GetCurrentWindow <> nil) and (not GetCurrentWindow.Hide);
end;

procedure TGUIFormList.CurrentFormDeactivate;
begin
  if Assigned(FCurrentForm) then
    TGUIForm(FCurrentForm).OnDeactivateForm;
end;

destructor TGUIFormList.Destroy;
var i: integer;
begin
  if Assigned(FMainMenu) then
    FreeAndNil(FMainMenu);

  if Assigned(FFormList) then
    for i := 0 to FFormList.Count - 1 do
    begin
      TGUIForm(FFormList[i]).Free;
      FFormList[i]:= nil;
    end;

  FreeAndNil(FFormList);

  inherited;
end;

procedure TGUIFormList.ClearNullReference;
begin
  FFormList.Pack;
end;

function TGUIFormList.CloseForm(index: integer): Boolean;
begin
  Result:= False;

  if not IsExists(index) then
    Exit;

  if TGUIForm(FFormList.Items[index]).OnClose then
  begin
    FFormList.Items[index]:= nil;
    Result:= True;
  end;
end;

function TGUIFormList.ConvertMouseButton(MouseButton: TMouseButton): TGUIMouseButton;
begin
  Result:= gmbNone;

  case MouseButton of
    mbLeft  : Result:= gmbLeft;
    mbRight : Result:= gmbRight;
    mbMiddle: Result:= gmbMiddle;
  end;

end;

function TGUIFormList.Count: Integer;
begin
  Result:= -1;

  if not Assigned(FFormList) then
    Exit;

  Result:= FFormList.Count;
end;

function TGUIFormList.GetCurrentWindow: TGUIForm;
begin
  Result:= nil;

  if not Assigned(FCurrentForm) then
    Exit;

  Result:= TGUIForm(FCurrentForm);
end;

function TGUIFormList.GetForm(pName: string): TGUIForm;
var FID: Integer;
begin
  Result:= nil;

  for FID := 0 to FFormList.Count - 1 do
    if SameText(TGUIForm(FFormList.Items[FID]).Name, pName) then
    begin
      Result:= TGUIForm(FFormList.Items[FID]);
      Break;
    end;
end;

function TGUIFormList.GetForm(id: integer): TGUIForm;
begin
  Result:= nil;

  if not IsExists(id) then
    Exit;

  Result:= TGUIForm(FFormList.Items[id]);
end;

function TGUIFormList.OnHit(pX, pY: Integer): Boolean;
var FID : Integer;
    Form: TGUIForm;
    FHit: Boolean;
begin
  Result:= False;

  //Проверим сначала у текущей формы
  if Assigned(TGUIForm(FCurrentForm)) then
    Result:= TGUIForm(FCurrentForm).OnHit(pX, pY);

  if Result then
    Exit;

  if not FMouseDown then
    Exit;

  for FID := 0 to Count - 1 do
  begin
    Form:= TGUIForm(FFormList.Items[FID]);

    //Если форма скрыта то пропускаем
    if Form.Hide then
      Continue;

    FHit:= Form.OnHit(pX, pY);

    //Если не попали по форме
    if not FHit then
    begin
      Form.OutHit(pX, pY);
      Continue;
    end;

    SetActive(FID);
    Result:= True;
    Break;
  end;

end;

procedure TGUIFormList.OnKeyDown(var Key: Word; Shift: TShiftState);
begin
  if not CurrentFormAccessible then
    Exit;

  GetCurrentWindow.OnKeyDown(Key, Shift);
end;

procedure TGUIFormList.OnKeyPress(Key: Char);
begin
  if not CurrentFormAccessible then
    Exit;

  GetCurrentWindow.OnKeyPress(Key);
end;

procedure TGUIFormList.OnKeyUp(var Key: Word; Shift: TShiftState);
begin
  if not CurrentFormAccessible then
    Exit;

  GetCurrentWindow.OnKeyUp(Key, Shift);
end;

function TGUIFormList.OnMouseDoubleClick(pX, pY: Integer; Button: TMouseButton): Boolean;
var MButton: TGUIMouseButton;
begin
  Result:= False;

  if not CurrentFormAccessible then
    Exit;

  MButton:= ConvertMouseButton(Button);
  GetCurrentWindow.OnMouseDoubleClick(pX, pY, MButton);

  Result:= True;
end;

function TGUIFormList.OnMouseDown(pX, pY: Integer; Button: TMouseButton): Boolean;
var MButton: TGUIMouseButton;
begin
  FMouseDown:= True;
  Result    := False;
  MButton   := ConvertMouseButton(Button);

  if Assigned(FMainMenu) then
  begin
    FMainMenu.OnMouseDown(pX, pY, MButton);

    if FMainMenu.ObjectInAction([goaFocused]) then
    begin
      CurrentFormDeactivate;
      Exit;
    end
  end;

  if not OnHit(pX, pY) then
    Exit;

  if not CurrentFormAccessible then
    Exit;

  GetCurrentWindow.OnMouseDown(pX, pY, MButton);
  Result:= True;
end;

function TGUIFormList.OnMouseMove(pX, pY: Integer): Boolean;
begin
  Result:= False;

  if Assigned(FMainMenu) then
  begin
    FMainMenu.OnMouseMove(pX, pY);
    if goaFocused in FMainMenu.GetAction then
      Exit;
  end;

  if not CurrentFormAccessible then
    Exit;

  GetCurrentWindow.OnMouseMove(pX, pY);

  Result:= True;
end;

function TGUIFormList.OnMouseUp(pX, pY: Integer; Button: TMouseButton): Boolean;
var MButton: TGUIMouseButton;
begin
  FMouseDown:= False;
  MButton   := ConvertMouseButton(Button);
  Result    := False;

  if Assigned(FMainMenu) then
    if FMainMenu.ObjectInAction([goaFocused, goaItemClick]) then
    begin
      FMainMenu.OnMouseUp(pX, pY, MButton);
      Exit;
    end;

  //Не будем проверять OnHit т.к. надо будет всем компонентам отправить MouseUp
  if not CurrentFormAccessible then
    Exit;

  GetCurrentWindow.OnMouseUp(pX, pY, MButton);

  Result:= True;
end;

function TGUIFormList.OnMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result:= False;
  if not CurrentFormAccessible then
    Exit;

  GetCurrentWindow.OnMouseWheelDown(Shift, MousePos.X, MousePos.Y);
  Result:= True;
end;

function TGUIFormList.OnMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result:= False;
  if not CurrentFormAccessible then
    Exit;

  GetCurrentWindow.OnMouseWheelUp(Shift, MousePos.X, MousePos.Y);
  Result:= True;
end;

procedure TGUIFormList.Render;
var FID: Integer;
begin
  for FID := Count - 1 downto 0 do
    if TGUIForm(FFormList.Items[FID]).Hide then
      Continue
    else
      TGUIForm(FFormList.Items[FID]).Render;

  if Assigned(FMainMenu) then
    FMainMenu.Render;

end;

procedure TGUIFormList.SetActive(pFormID: Integer);
begin
  if not IsExists(pFormID) then
    Exit;

  try
    //Всем компонентам старой формы отключаем фокус
    CurrentFormDeactivate;

    //Перемещаем форму на передний план
    //Последняя рисуется первой
    FFormList.Move(pFormID, 0);
    //Устанавливаем ссылку на текущую форму
    FCurrentForm:= FFormList.Items[0];
  except

  end;
end;

procedure TGUIFormList.SetActive(pForm: TGUIForm);
begin
  if not Assigned(pForm) then
    Exit;

  SetActive(pForm.Name);
end;

function TGUIFormList.WndProc(var AMessage: TMessage): Boolean;
const SCROLL_UP   = 120;
      SCROLL_DOWN = 65416;

var AKey: Word;
    Shift: TShiftState;
begin
  Result:= False;

  case AMessage.Msg of
    //Левая кнопка мыши
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
    begin
      //Нужно запомнить для скорллла
      FLastMousePos.X:= AMessage.LParamLo;
      FLastMousePos.Y:= AMessage.LParamHi;

      Result:= OnMouseDown(AMessage.LParamLo, AMessage.LParamHi, mbLeft);

      if AMessage.Msg = WM_LBUTTONDBLCLK then
        Result:= OnMouseDoubleClick(AMessage.LParamLo, AMessage.LParamHi, mbLeft);
    end;

    WM_LBUTTONUP:
      Result:= OnMouseUp(AMessage.LParamLo, AMessage.LParamHi, mbLeft);

    //Правая кнопка мыши
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
    begin
      Result:= OnMouseDown(AMessage.LParamLo, AMessage.LParamHi, mbRight);
      //Нужно запомнить для скорллла
      FLastMousePos.X:= AMessage.LParamLo;
      FLastMousePos.Y:= AMessage.LParamHi;

      if AMessage.Msg = WM_RBUTTONDBLCLK then
        Result:= OnMouseDoubleClick(AMessage.LParamLo, AMessage.LParamHi, mbRight);
    end;

    WM_RBUTTONUP:
      Result:= OnMouseUp(AMessage.LParamLo, AMessage.LParamHi, mbRight);

    WM_MOUSEWHEEL:
      begin
        Result:= False;

        case AMessage.WParamHi of
          SCROLL_UP  : Result:= OnMouseWheelUp  (Shift, FLastMousePos);
          SCROLL_DOWN: Result:= OnMouseWheelDown(Shift, FLastMousePos);
        end;
      end;

    WM_MOUSEMOVE:
      Result:= OnMouseMove(AMessage.LParamLo, AMessage.LParamHi);

    WM_KEYDOWN:
    begin
      AKey:= AMessage.WParamLo;
      if GetKeyState(VK_SHIFT)   < 0 then
        Shift:= Shift + [ssShift];
      if GetKeyState(VK_CONTROL) < 0 then
        Shift:= Shift + [ssCtrl];
      if (AKey and $20000000)   <> 0 then
        Shift:= Shift + [ssAlt];

      OnKeyDown(AKey, Shift);
    end;

    WM_CHAR:
      OnKeyPress(Chr(AMessage.WParamLo));

    WM_KEYUP:
    begin
      AKey:= AMessage.WParamLo;
      OnKeyUp(AKey, Shift);
    end;

    WM_SIZE:
    begin
      if Assigned(FMainMenu) then
        FMainMenu.ResizeWnd(AMessage.LParamLo);
    end;
  end;

  Dispatch(AMessage);
end;

procedure TGUIFormList.SetActive(pFormName: String);
var FID: Integer;
begin
  for FID := 0 to FFormList.Count - 1 do
    if SameText(pFormName, TGUIForm(FFormList.Items[FID]).Name) then
    begin
      SetActive(FID);
      Break;
    end;
end;

function TGUIFormList.IsExists(pNum: integer): Boolean;
begin
  Result:= (Assigned(FFormList) and (pNum > -1) and (pNum < Count));
end;

initialization
   FormList:= TGUIFormList.Create;

finalization
   if Assigned(FormList) then
     FreeAndNil(FormList);

end.
