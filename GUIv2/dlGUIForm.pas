unit dlGUIForm;

interface

 uses Windows, dlGUITypes, dlOpenGL, Graphics, SysUtils, Classes, dlGUIObject, dlGUIFont,
      dlGUIPopupMenu, dlGUIButton, dlGUIPaletteHelper;

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

  {
    * Read Me *

     Для того чтобы указывать просто координаты на текстуре нужно указать
     TexSizeW - макс ширина текстуры, TextSizeH - макс высота текстуры
     и GUI автоматически будет вычислять координаты

  }

 const HEIGHT_CAPTION = 20; //Высота заголовка
       ID_BTN_CLOSE   = 1;  //Идентификаторы кнопок в массиве
       ID_BTN_MINIM   = 2;
       BUTTON_WIDTH   = 18; //Размеры кнопки

 type
   //Хинт на форме
   TGUIFormHint = class(TGUIObject)
     private
       FText     : String;
       FLastTime : TUInt;
       FWaitStart: TUInt;
       FShowTime : TUInt;
     protected
       procedure SetColor(pColor: TColor); override;
     private
       procedure SetText(pText: String);
     public
       constructor Create(pTextureFont: TTextureLink = nil);
       procedure SetHide(pHide: Boolean); override;
       procedure Render; override;
     public
       property Text: String read FText write SetText;
   end;

   TGUIBorderIcons = (fbiAll, fbiClose, fbiMinimized);
   TGUIFormStyle = (fglNormal, fglNone);

   //Заголовок окна
   TGUIFormCaption = class(TGUIObject)
     private
       //Для перемещения окна
       FDrag    : Boolean;
       FLastPosX: Integer;
       FLastPosY: Integer;
       FText    : String; //Название окна
       FButton  : array[1..2] of TGUIButton;
     private
       procedure MakeButton(var pButton: TGUIButton; pProc: TGUIProc; pImage, pImageDown: Integer);
       procedure CalculateButtonPos;
       procedure HideWindow(Sender: TObject; ParamObj: Pointer = nil);
       procedure MinimizeWindow(Sender: TObject; ParamObj: Pointer = nil);
     protected
       procedure SetResize; override;
       procedure SetMoveMousePos(pX, pY: Integer);
     public
       constructor Create(pName, pCaption: String; pX, pY, pWidth, pHeight: Integer; pTextureLink: TTextureLink = nil; pTextureFont: TTextureLink = nil;
          pBorderIcons: TGUIBorderIcons = fbiAll);
       destructor Destroy; override;

       procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
       procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
       procedure OnMouseMove(pX, pY: Integer); override;
       procedure OnMouseOver(pX, pY: Integer); override;
       procedure Render; override;
       procedure RenderText; override;
   end;

   //Окно
   TGUIForm = class(TGUIObject)
     private
       //Заголовок
       FCaption     : TGUIFormCaption;
       FMinimized   : Boolean;
       FHitCaption  : Boolean; //OnHit сработал у Caption или у Form
     private
       FCurrID      : Integer; //Текущий индекс компонента, номер последнего добавленного
       FActivePopup : TGUIPopupMenu; //Текущее активное меню
       FFocusComp   : TGUIObject; //Компонент в фокусе
     private
       FHint        : TGUIFormHint; //Всплывающая подсказка
     private
       FComponent   : TList;   //Список компонентов

       function GetCaption: String;
       procedure SetCaption(pCaption: String);
       procedure DestroyActivePopup;
     protected
       procedure SetResize; override;
     public
       constructor Create(pName, pCaption: String; pX, pY, pWidth, pHeight: Integer; pTextureLink: TTextureLink = nil; pTextureFont: TTextureLink = nil; pFormStyle: TGUIFormStyle = fglNormal;
         pBorderIcons: TGUIBorderIcons = fbiAll);
       destructor Destroy; override;

       function IsExists(pIndex: integer): Boolean;

       procedure AddComponent(pComponent: TGUIObject);
       function GetComponent(pName: String): TGUIObject; overload;
       function GetComponent(pIndex: integer): TGUIObject; overload;
       function GetComponentList: String;

       procedure OutHit(pX, pY: Integer); override;
       function  OnHit(pX, pY: Integer): Boolean; override;
       procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
       procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
       procedure OnMouseMove(pX, pY: Integer); override;
       procedure OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer); override;
       procedure OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer); override;
       procedure OnKeyDown(var Key: Word; Shift: TShiftState); override;
       procedure OnKeyUp(var Key: Word; Shift: TShiftState); override;
       procedure OnKeyPress(Key: Char); override;

       procedure OnDeactivateForm; //Форма больше не активная
       function OnClose: Boolean; //Событие закрытия формы

       procedure AfterObjRender; override;
       procedure Render; override;
       procedure SendGUIMessage(pMessage: TGUIMessage); override;

       procedure SetX(value: integer);
       function GetX: Integer;
       procedure SetY(value: integer);
       function GetY: Integer;
     public
       property CaptionObject: TGUIFormCaption   read FCaption    write FCaption;
       property ComponentList: TList             read FComponent;
       property Item[index: integer]: TGUIObject read GetComponent;
       property HintObject: TGUIFormHint         read FHint;
     published
       property Caption: String                  read GetCaption  write SetCaption;
       property ObjectType;
       property Name;
       property X: Integer read GetX write SetX;
       property Y: Integer read GetY write SetY;
       property Width;
       property Height;
       property Color;
       property Font;
       property Hide;
       property TextureName;
       property Minimize: Boolean                read FMinimized  write FMinimized;
       //классы
       property PopupMenuName;
       property Parent;
       property Hint;
       property Blend;

   end;

implementation

{ TGUIFormCaption }

procedure TGUIFormCaption.CalculateButtonPos;
var FID : integer;
    Left: Integer;
begin
  Left:= 2;
  for FID := Low(FButton) to High(FButton) do
   if Assigned(FButton[FID]) then
   begin
     Left:= Left + FButton[FID].Width;
     FButton[FID].X:= Rect.X + Rect.Width - Left;
     FButton[FID].Y:= Rect.Y + 1;
   end;
end;

constructor TGUIFormCaption.Create(pName, pCaption: String; pX, pY, pWidth, pHeight: Integer; pTextureLink: TTextureLink = nil; pTextureFont: TTextureLink = nil;
  pBorderIcons: TGUIBorderIcons = fbiAll);
begin
  inherited Create(pName);

  FDrag       := False;
  FLastPosX   := 0;
  FLastPosY   := 0;
  Color       := clWhite;
  FText       := pCaption;

  SetRect(pX, pY, pWidth, pHeight);
  SetTextureLink(pTextureLink);
  FFont.SetTextureLink(pTextureFont);
  FTextOffset.SetRect(2, 2, 0, 0);

  //Создаем кнопки
  if (fbiAll in [pBorderIcons])   or
     (fbiClose in [pBorderIcons]) then
     MakeButton(FButton[ID_BTN_CLOSE], HideWindow, pal_BtnCloseUp, pal_BtnCloseDn);

  if (fbiAll in [pBorderIcons])       or
     (fbiMinimized in [pBorderIcons]) then
     MakeButton(FButton[ID_BTN_MINIM], MinimizeWindow, pal_BtnMinimizeUp, pal_BtnMinimizeDn);

  //Пересчитываем позицию кнопки
  CalculateButtonPos;

  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height,
     clWhite, clWhite, clGray, clGray, GUIPalette.GetCellRect(pal_Frame));

  VertexList.MakeSquareOffset(0, 1,
     clGray, clWhite, clWhite, clGray, GUIPalette.GetCellRect(pal_Window));
end;

destructor TGUIFormCaption.Destroy;
var i: integer;
begin
  for i:= Low(FButton) to High(FButton) do
    if Assigned(FButton[i]) then
      FButton[i].Free;

  inherited;
end;

procedure TGUIFormCaption.HideWindow(Sender: TObject; ParamObj: Pointer = nil);
begin
  Hide:= True;
  Parent.Hide:= True;
end;

procedure TGUIFormCaption.MakeButton(var pButton: TGUIButton; pProc: TGUIProc; pImage, pImageDown: Integer);
begin
  pButton:= TGUIButton.Create('', '', 0, 0, Self.GetTextureLink);
  pButton.Rect.SetSize(BUTTON_WIDTH, BUTTON_WIDTH);
  pButton.Area.Show:= False;
  pButton.OnResize;
  pButton.OnClick:= pProc;
  pButton.VertexList.SetVertexTextureMap( 4, GUIPalette.GetCellRect(pImage));
  pButton.VertexList.SetVertexTextureMap(12, GUIPalette.GetCellRect(pImageDown));
end;

procedure TGUIFormCaption.MinimizeWindow(Sender: TObject; ParamObj: Pointer = nil);
begin
  TGUIForm(Parent).FMinimized:= not TGUIForm(Parent).FMinimized;
end;

procedure TGUIFormCaption.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var FID: Integer;
begin
  if Button <> gmbLeft then
    Exit;

  if not OnHit(pX, pY) then
    Exit;

  SetMoveMousePos(pX, pY);

  for FID := Low(FButton) to High(FButton) do
    if Assigned(FButton[FID]) then
      if FButton[FID].OnHit(pX, pY) then
      begin
        FButton[FID].OnMouseDown(pX, pY, Button);
        Exit;
      end;

  FDrag:= True;
end;

procedure TGUIFormCaption.OnMouseMove(pX, pY: Integer);
var FID : Integer;
begin
  if FDrag then
  begin
    //Установим новую позицию заголовка
    Rect.SetPos(
       FRect.X - (FLastPosX - pX),
       FRect.Y - (FLastPosY - pY)
    );

    //
    if Rect.X < 0 then X:= 0;
    if Rect.Y < 0 then Y:= 0;

    //Установим LastPosX, Y новые значения
    SetMoveMousePos(pX, pY);
    //Пересчитаем позицию кнопок
    CalculateButtonPos;

    Exit;
  end;

  //Отправим сообщение OnMouseMove на наши кнопки
  if not OnHit(pX, pY) then
    Exit;

  for FID := Low(FButton) to High(FButton) do
    if Assigned(FButton[FID]) then
      FButton[FID].OnMouseMove(pX, pY);
end;

procedure TGUIFormCaption.OnMouseOver(pX, pY: Integer);
var FID: Integer;
begin
  for FID := Low(FButton) to High(FButton) do
    if Assigned(FButton[FID]) then
      FButton[FID].OnMouseOver(pX, pY);
end;

procedure TGUIFormCaption.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var FID: Integer;
begin
  FDrag:= False;

  //Отправим сообщения OnMouseUp на наши кнопки
  for FID := Low(FButton) to High(FButton) do
    if Assigned(FButton[FID]) then
       FButton[FID].OnMouseUp(pX, pY, Button);
end;

procedure TGUIFormCaption.Render;
var FID: Integer;
begin
  if Hide then
    Exit;

  //Нарисуем заголовок
  inherited;

  //Нарисуем кнопки
  for FID := Low(FButton) to High(FButton) do
    if Assigned(FButton[FID]) then
      FButton[FID].Render;
end;

procedure TGUIFormCaption.RenderText;
begin
  //Стандартная подготовка текста
  inherited;
  //Вывод текста заголовка
  FFont.RenderText(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FText, Rect.Width);
end;

procedure TGUIFormCaption.SetMoveMousePos(pX, pY: Integer);
begin
  FLastPosX:= pX;
  FLastPosY:= pY;
end;

procedure TGUIFormCaption.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 1, 1, Rect.Width - 2, Rect.Height - 2);

  CalculateButtonPos;
end;

{ TGUIForm }

constructor TGUIForm.Create(pName, pCaption: String; pX, pY, pWidth, pHeight: Integer; pTextureLink: TTextureLink = nil; pTextureFont: TTextureLink = nil;
  pFormStyle: TGUIFormStyle = fglNormal; pBorderIcons: TGUIBorderIcons = fbiAll);
begin
  inherited Create(pName, gtcForm);
  FComponent:= TList.Create;

  //Создание заголовка
  if pFormStyle = fglNormal then
  begin
    FCaption:= TGUIFormCaption.Create('FormCaption', pCaption, pX, pY, pWidth, HEIGHT_CAPTION, pTextureLink, pTextureFont, pBorderIcons);
    FCaption.Parent:= Self;
  end;

  //Создание хинта
  FHint:= TGUIFormHint.Create(pTextureFont);

  //Создание формы
  FCurrID     := 0;
  FMinimized  := False;
  FActivePopup:= nil;
  FFocusComp  := nil;
  Color       := clWhite;

  SetRect(pX, pY, pWidth, pHeight);
  SetTextureLink(pTextureLink);
  FFont.SetTextureLink(pTextureFont);

  //Рамка
  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_Frame));
  //Основная часть
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_Window));
end;

destructor TGUIForm.Destroy;
var i: integer;
begin
  if Assigned(FCaption) then
    FreeAndNil(FCaption);

  if Assigned(FHint) then
    FreeAndNil(FHint);

  if Assigned(FBlend) then
    FreeAndNil(FBlend);

  for I := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).Free;

  FFocusComp:= nil;
  FreeAndNil(FComponent);

  inherited;
end;

procedure TGUIForm.SetResize;
begin
  //Основная часть
  VertexList.SetVertexPosSquare(0, 0, 0, Width, Height);
  VertexList.SetVertexPosSquare(4, 1, 0, Width - 2, Height - 1);

  if not Assigned(FCaption) then
    Exit;

  FCaption.Width:= Width;
end;

procedure TGUIForm.SetX(value: integer);
begin
  if Assigned(FCaption) then
  begin
    FCaption.Rect.SetPos(value, FCaption.Rect.Y);
    FCaption.CalculateButtonPos;
  end
  else
    Rect.SetPos(value, Rect.Y);
end;

procedure TGUIForm.SetY(value: integer);
begin
  if Assigned(FCaption) then
  begin
    FCaption.Rect.SetPos(FCaption.Rect.X, value);
    FCaption.CalculateButtonPos;
  end
  else
    Rect.SetPos(Rect.X, value);
end;

procedure TGUIForm.AddComponent(pComponent: TGUIObject);
var msg: TGUIMessage;
begin
  if not Assigned(pComponent) then
    Exit;

  //Если нет текстуры то присваиваем
  if (pComponent.GetTextureLink = nil) then
    pComponent.SetTextureLink(Self.GetTextureLink);

  //Если нет шрифта то присваиваем
  if pComponent.Font.GetTextureLink = nil then
    pComponent.Font.CopyFrom(Self.FFont);

  msg.Msg := MSG_FORM_INSERTOBJ;
  msg.Self:= self;

  inc(FCurrID);
  pComponent.ID    := FCurrID;
  pComponent.Parent:= Self;
  FComponent.Add(pComponent);

  //Отправка компоненту уведомления что добавили на форму
  pComponent.SendGUIMessage(msg);
end;

procedure TGUIForm.DestroyActivePopup;
begin
  if Assigned(FActivePopup) then
    FActivePopup.Hide:= True;

  FActivePopup:= nil;
end;

procedure TGUIForm.SendGUIMessage(pMessage: TGUIMessage);
var FID: Integer;
begin
  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).SendGUIMessage(pMessage);
end;

function TGUIForm.GetCaption: String;
begin
  Result:= '';

  if not Assigned(FCaption) then
    Exit;

  Result:= FCaption.FText;
end;

function TGUIForm.GetComponent(pName: String): TGUIObject;
var FID: Integer;
begin
  Result:= nil;

  if not Assigned(FComponent) then
    Exit;

  for FID := 0 to FComponent.Count - 1 do
    if SameText(TGUIObject(FComponent.Items[FID]).Name, pName) then
    begin
      Result:= TGUIObject(FComponent.Items[FID]);
      Break;
    end;
end;

function TGUIForm.GetComponent(pIndex: integer): TGUIObject;
begin
  Result:= nil;

  if not IsExists(pIndex) then
    Exit;

  Result:= TGUIObject(ComponentList.Items[pIndex]);
end;

function TGUIForm.GetComponentList: String;
var Buf: TStringList;
    FID: Integer;
begin
  Buf:= TStringList.Create;

   for FID := 0 to FComponent.Count - 1 do
     Buf.Add(TGUIObject(FComponent[FID]).Name);

  Result:= Buf.Text;
  Buf.Free;
end;

function TGUIForm.GetX: Integer;
begin
  if Assigned(FCaption) then
    Result:= FCaption.Rect.X
  else
    Result:= Rect.X;
end;

function TGUIForm.GetY: Integer;
begin
  if Assigned(FCaption) then
    Result:= FCaption.Rect.Y
  else
    Result:= Rect.Y;
end;

function TGUIForm.IsExists(pIndex: integer): Boolean;
begin
  Result:= (pIndex > -1) and (pIndex < ComponentList.Count);
end;

function TGUIForm.OnClose: Boolean;
begin
  Result:= True;
end;

procedure TGUIForm.OnDeactivateForm;
var FID: Integer;
begin
  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).OnDeactivate(Self);

  FHint.Hide:= True;
end;

function TGUIForm.OnHit(pX, pY: Integer): Boolean;
begin
  Result:= False;

  FHitCaption:= False;
  if Hide then
    Exit;

  //Проверим заголовок
  if Assigned(FCaption) then
  begin
    Result:= FCaption.OnHit(pX, pY);
    FHitCaption:= Result;
  end;

  if Result then
    Exit;

  //Проверим саму форму
  if not FMinimized then
    Result:= inherited OnHit(pX, pY);

  if Result then
    Exit;

  //Проверим фокусный компонент
  if Assigned(FFocusComp) then
    Result:= FFocusComp.OnHit(pX - Rect.X, pY - Rect.Y);

  if Result then
    Exit;

  //Проверим может быть на PopupMenu нажали
  if Assigned(FActivePopup) then
    if not FActivePopup.Hide then
      Result:= FActivePopup.OnHit(pX - Rect.X, pY - Rect.Y);
end;

procedure TGUIForm.OnKeyDown(var Key: Word; Shift: TShiftState);
var FID: Integer;
begin
  if Hide then
    Exit;

  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).OnKeyDown(Key, Shift);
end;

procedure TGUIForm.OnKeyPress(Key: Char);
var FID: Integer;
begin
  if Hide then
    Exit;

  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).OnKeyPress(Key);
end;

procedure TGUIForm.OnKeyUp(var Key: Word; Shift: TShiftState);
var FID: Integer;
begin
  if Hide then
    Exit;

  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).OnKeyUp(Key, Shift);
end;

procedure TGUIForm.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);

  procedure GetPopupMenu(pPopupMenu: TGUIObject; pX, pY: Integer; pButton: TGUIMouseButton);
  begin
    if not Assigned(pPopupMenu) then
      Exit;

    //Если нашли назначаем ActivePopup меню этого компонента
    FActivePopup:= TGUIPopupMenu(pPopupMenu);
    FActivePopup.OnMouseDown(pX, pY, pButton);

    //Если меню не раскрылось то очищаем текущее меню
    if FActivePopup.Hide then DestroyActivePopup;
  end;

var FID: Integer;
    RX, RY: Integer; //Текущие координаты
    Obj: TGUIObject; //Объект в списке
begin
  if Hide then Exit;

  RX:= pX - Rect.X;
  RY:= pY - Rect.Y;

  //Проверим нажали ли мы на что нибудь в заголовке или нет
  if Assigned(FCaption) then
  begin
    FCaption.OnMouseDown(pX, pY, Button);
    if FHitCaption then
      DestroyActivePopup;
  end;

  //Если мы скрыли форму при нажатии на Caption то выходим
  if FMinimized then
    Exit;

  //Если есть уже раскрытое меню то нажимаем только на него и выходим
  if (Assigned(FActivePopup)) and (not FActivePopup.Hide) then
  begin
    if FActivePopup.OnHit(RX, RY) then
    begin
      FActivePopup.OnMouseDown(RX, RY, Button);
      Exit;
    end
    else DestroyActivePopup;
  end
  else DestroyActivePopup; //Очищаем текущее меню

  FActivePopup:= TGUIPopupMenu(PopupMenu);

  //Unfocused все компоненты
{  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).RemoveAction([goaFocused]);}

  FFocusComp:= nil;

  //Перебираем все компоненты на форме
  for FID := FComponent.Count - 1 downto 0 do
  begin
    Obj:= TGUIObject(FComponent.Items[FID]);

    if not Obj.OnHit(RX, RY) then
    begin
      Obj.OutHit(RX, RY);
      Continue;
    end;

    Obj.SetAction([goaFocused]);
    FFocusComp:= Obj;

    //Передадим на событие до появления меню например для изменения имени пункта меню
    Obj.BeforeOnMouseDown(RX, RY, Button);

    //Если было до этого раскрыто какое то PopupMenu то скроем его
    DestroyActivePopup;

    //Проверяем у компонента Popup меню
    FActivePopup:= TGUIPopupMenu(Obj.PopupMenu);

    //Передаем нажатие на компонент
    //Если поставить OnMouseDown компонента раньше чем PopupMenu то
    //При получении сигнала OnMouseDown будет пустое состояние PopupMenu.State
    Obj.OnMouseDown(RX, RY, Button);

    //Получить какой то другой активный popup например у ListBox
    if not Assigned(FActivePopup) then
      FActivePopup:= TGUIPopupMenu(Obj.GetChildItemPopup);
  end;

  if (not FHitCaption) and Assigned(FActivePopup) then
    FActivePopup.OnMouseDown(RX, RY, Button)
  else
    DestroyActivePopup;

  //Для вычисления позиции при перемещении формы
  if Assigned(FCaption) then
    FCaption.SetMoveMousePos(pX, pY);
end;

procedure TGUIForm.OnMouseMove(pX, pY: Integer);
var FID: Integer;
    Obj: TGUIObject;
    Hit: Boolean;
begin
  if Hide then
    Exit;

  //Если мышь над заголовком
  if (Assigned(FCaption)) then
  begin
    if not FCaption.OnHit(pX, pY) then
      FCaption.OnMouseOver(pX - Rect.X, pY - Rect.Y);

    FCaption.OnMouseMove(pX, pY);

    if FCaption.FDrag then
      Exit;
  end;

  //Окно свернуто
  if FMinimized then
    Exit;

  //Скрываем хинт если показан
  FHint.Hide:= True;
  Hit:= False;

  //Для элемента в фокусе всегда OnMouseMove (для TList) например
  if Assigned(FFocusComp) then
  begin
    FFocusComp.OnMouseMove(pX - Rect.X, pY - Rect.Y);
      //if FFocusComp.OnHit(pX - Rect.X, pY - Rect.Y) then
      //  Hit:= True;
  end;

  for FID:= 0 to FComponent.Count - 1 do
  begin
    Obj:= TGUIObject(FComponent.Items[FID]);

    if Obj.Hide then
      Continue;

    if not Hit then
      if Obj.OnHit(pX - Rect.X, pY - Rect.Y) then
      begin
        Obj.OnMouseMove(pX - Rect.X, pY - Rect.Y);
        Hit:= True;

        if Obj.Hint.Enable then
          if Assigned(FHint) and (FHint.Hide) then
          begin
            //Обрабатываем хинт
            FHint.Color:= Obj.Hint.BackgroundColor;
            FHint.Hide := False;
            FHint.Text := Obj.Hint.Text;
            FHint.Font.Color:= Obj.Hint.Color;
            FHint.Rect.SetPos(pX - Rect.X + 14, pY - Rect.Y + 5);
          end;

        Continue;
      end;

     if (not Assigned(FFocusComp)) or (FFocusComp <> Obj) then
       Obj.OnMouseOver(pX - Rect.X, pY - Rect.Y)
  end;

  //Если есть активный PopupMenu пошлем сообщение
  if Assigned(FActivePopup) then
     FActivePopup.OnMouseMove(pX - Rect.X, pY - Rect.Y);

end;

procedure TGUIForm.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var FID: Integer;
begin
  if Hide then Exit;

  if Assigned(FCaption) then
    FCaption.OnMouseUp(pX, pY, Button);

  if FMinimized then Exit;

  for FID := 0 to FComponent.Count - 1 do
     TGUIObject(FComponent.Items[FID]).OnMouseUp(pX - Rect.X, pY - Rect.Y, Button);

  if Assigned(FActivePopup) then
     FActivePopup.OnMouseUp(pX - Rect.X, pY - Rect.Y, Button);
end;

procedure TGUIForm.OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer);
var FID: Integer;
begin
  if Hide then Exit;

  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).OnMouseWheelDown(Shift, MPosX - Rect.X, MPosY - Rect.Y);
end;

procedure TGUIForm.OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer);
var FID: Integer;
begin
  if Hide then Exit;

  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).OnMouseWheelUp(Shift, MPosX - Rect.X, MPosY - Rect.Y);
end;

procedure TGUIForm.OutHit(pX, pY: Integer);
begin
  if Assigned(FFocusComp) then
    FFocusComp.OutHit(pX, pY);

  DestroyActivePopup;
end;

procedure TGUIForm.Render;
begin
  if Hide then Exit;

  if not Assigned(FCaption) then
  begin

    //Если нет заголовка то рисуем так
    if not FMinimized then
     inherited;

    Exit;
  end;

  //Если заголовок есть
  Rect.SetPos(
     FCaption.Rect.X,
     FCaption.Rect.Y + FCaption.Rect.Height
  );

  //Если окно не свернуто то прорисовываем как есть через GUIObject.Render
  if not FMinimized then
   inherited;

  //Прорисовываем заголовок
  FCaption.Render;
end;

procedure TGUIForm.AfterObjRender;
var FID: Integer;
begin
  if Hide then Exit;

  //Рисуем компоненты на форме
  for FID := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[FID]).Render;

  //Отображаем PopupMenu
  if Assigned(FActivePopup) then
    FActivePopup.Render;

  //Отображаем всплывающую подсказку
  if Assigned(FHint) then
    FHint.Render;
end;

procedure TGUIForm.SetCaption(pCaption: String);
begin
  if Assigned(FCaption) then
    FCaption.FText:= pCaption;
end;

{ TGUIFormHint }

constructor TGUIFormHint.Create(pTextureFont: TTextureLink);
begin
  inherited Create('Hint', gtcObject);

  Text:= '';
  FFont.SetTextureLink(pTextureFont);

  FTextOffset.X:= 2;
  FTextOffset.Y:= 2;

  //Параметры цвета передаются от TGUIHintObject
  //тут их нет смысла устанавливать

  SetRect(0, 0, 1, 1);

  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, 0, nil, 0);
  VertexList.MakeSquare(1, 1, Rect.Width, Rect.Height, Color, nil, 1);
end;

procedure TGUIFormHint.Render;
begin
  if Hide then
    Exit;

  if GetCurrentTime - FLastTime < FWaitStart then Exit;
  if GetCurrentTime - FLastTime > FShowTime  then Exit;

  inherited;

  FFont.RenderText(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FText, 0, True);
end;

procedure TGUIFormHint.SetColor(pColor: TColor);
begin
//  inherited;
  FColor.SetColor(pColor);
  VertexList.SetGroupColor(1, pColor);
end;

procedure TGUIFormHint.SetHide(pHide: Boolean);
begin
  inherited;

  if FHide then
  begin
    FLastTime := GetCurrentTime;
    FWaitStart:= 500;
    FShowTime := 15000;
  end;
end;

procedure TGUIFormHint.SetText(pText: String);
var outWidth, outHeight: Integer;
begin
  if not SameText(FText, pText) then
  begin
    FFont.GetTextRect(pText, outWidth, outHeight);
    Rect.SetSize(
       outWidth  + (FTextOffset.X * 4),
       outHeight + (FTextOffset.Y * 4)
    );

    //Размеры фона
    VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
    VertexList.SetVertexPosSquare(4, 1, 1, Rect.Width - 2, Rect.Height - 2);
  end;

  FText:= pText;
end;

initialization

end.
