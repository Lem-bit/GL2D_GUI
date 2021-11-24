unit dlGUIForm;

interface

uses Classes, SysUtils, Windows, Graphics, dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlOpenGL,
  dlGUIButton, dlGUIPopupMenu, dlGUIFont, dlGUIXmlSerial;

const MOUSE_AREA = 20; //Для Hint

type
  TGUIFormBorderStyle = (fsSizeable, fsNone, fsSingle);
  TGUIFormStatus = record
    public
      Text   : String;
      OffsetX: Integer;
      OffsetY: Integer;
    public
      procedure Clear;
  end;

  //Хинт на форме
  TGUIFormHint = class(TGUIObject)
    strict private
      FText     : String;
      FLastTime : TUInt;
      FWaitStart: TUInt;
      FShowTime : TUInt;
    protected
      procedure SetColor(pColor: TColor); override;
    strict private
      procedure SetText(pText: String);
    public
      constructor Create(pTextureFont: TTextureLink = nil);

      procedure SetHide(pHide: Boolean); override;
      procedure SetViewPort(pWidth, pHeight: Integer);
      procedure Render; override;
    public
      property Text: String read FText write SetText;
  end;

  TGUIFocusComp = class
    strict private
      FComp: TGUIObject;
    strict private
      procedure SetComponent(AObj: TGUIObject);
    public
      constructor Create;
      destructor Destroy; override;
    public
      function IsAssigned: Boolean;
      procedure Render;
    public
      property Comp: TGUIObject read FComp write SetComponent;
  end;

  TGUIForm = class(TGUIObject)
    strict private
      type TGUIActionItem = (aiNone, aiForm, aiCaption, aiStatus, aiStatusBtn, aiPopup);
      type TGUIFormProp = (fpNone, fpNeedRecalc);
    strict private
      const CAPTION_HEIGHT   = 21; //Высота заголовка
      const STATUS_HEIGHT    = 16; //Высота статус бара
      const STATUS_BTN_SIZE  = 16; //Размер кнопки ресайза

      const GROUP_CAPTION    = 1;
      const GROUP_FORM       = 2;
      const GROUP_STATUS_BAR = 3;
      const GROUP_STATUS_BTN = 4;

      const BUTTON_SIZE      = 18;
    strict private
      FPrevRect   : TGUIObjectRect;
      FStatus     : TGUIFormStatus;  //Текст в статусе

      FLastPosX   : Integer;
      FLastPosY   : Integer;

      FMinWidth   : Integer;
      FMinHeight  : Integer;

      FCaption    : String;  //Название формы
      FMinimize   : Boolean; //Форма свернута
      FIndex      : Integer; //Текущий индекс компонента
      FOffsetY    : Integer; //Для сдвига компонентов когда отображается Caption

      FDownAction : TGUIActionItem; //На что нажали мышью
      FBorderStyle: TGUIFormBorderStyle;

      FBtnMinimize: TGUIButton;
      FBtnHide    : TGUIButton;

      FHint       : TGUIFormHint;
      FFocused    : TGUIFocusComp;
      FActivePopup: TGUIPopupMenu;

      FViewPort   : array[0..3] of Integer;
      FProps      : TGUIFormProp;

      [TXMLSerial] FComponent: TList;   //Компоненты
    strict private
      procedure ShowHint(AComp: TGUIObject; AmP: TCoord2DI);

      procedure GetCurrentViewPort;
      procedure SetPosButtons;
      function CurrOnHit(pRect: TGUIObjectRect; pX, pY: Integer): Boolean;

      function GetRectCaption  : TGUIObjectRect;
      function GetRectStatus   : TGUIObjectRect;
      function GetRectForm     : TGUIObjectRect;
      function GetRectStatusBtn: TGUIObjectRect;
      function GetRectBtnMin   : TGUIObjectRect;
      function GetRectBtnHide  : TGUIObjectRect;

      //Найти компонент по его индексу
      function FindByGUID(pComponent: TGUIObject): Integer;
      function GetComponentByIndex(Index: Integer): TGUIObject;
      function GetComponentByStr(Index: String): TGUIObject;
      function Search(pName: String): Integer;
      function IndexExists(pIndex: integer): Boolean;

      procedure SetMinimize(pValue: Boolean);
      procedure SetBorderStyle(pValue: TGUIFormBorderStyle);

      procedure SetResizeForm(pX, pY: Integer);
      procedure MakeButton(var pButton: TGUIButton; pProc: TGUIProc; pImage, pImageDown: Integer);
      procedure OnHide(Sender: TObject; ParamObj: Pointer = nil);
      procedure OnMinimize(Sender: TObject; ParamObj: Pointer = nil);

      procedure DestroyActivePopup;
      function IsAssigned: Boolean;

      procedure ComponentAlignment(AOffsetRect: TGUIObjectRect);

      //Вычислить текущую позицию мыши на форме
      function GetCorrPos(pX, pY: Integer): TCoord2DI;
    protected
      procedure SetResize; override;
      procedure SetFontEvent; override;
    public
      constructor Create(pName: String; pTextureLink: TTextureLink = nil);
      destructor Destroy; override;
    public
      procedure OnDeactivateForm;
      function OnClose: Boolean;
    public
      procedure SetTextureLink(pTextureLink: TTextureLink); override;
      procedure BroadcastMessage(pMessage: TGUIMessage); override;
      procedure SetCenterByRect(pRect: TRect);

      function  OnHitByComponent(pX, pY: Integer): Boolean;
      function  OnHit(pX, pY: Integer): Boolean; override;
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OutHit(pX, pY: Integer); override;

      //Только компоненту в фокусе (FFocused.Comp)
      procedure OnKeyPress(Key: Char); override;
      procedure OnKeyDown(var Key: Word; Shift: TShiftState); override;
      procedure OnKeyUp(var Key: Word; Shift: TShiftState); override;
      procedure OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer); override;
      procedure OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer); override;
    private
      //Если нужно то можно и в public
      function HasActivePopup: Boolean; //Есть раскрытый активный popup
      function ShowPopupMenu(pPopupMenu: TGUIObject; pX, pY: Integer; pButton: TGUIMouseButton): Boolean;
      procedure FreeActivePopup;
    public
      function AddComponent(pComponent: TGUIObject): Boolean;
      function DelComponent(pComponent: TGUIObject): Boolean;
      procedure ProcessEvents;
    public
      [TXMLSerial] property Caption: String                  read FCaption     write FCaption;
      [TXMLSerial] property BorderStyle: TGUIFormBorderStyle read FBorderStyle write SetBorderStyle;
      [TXMLSerial] property Minimize: Boolean                read FMinimize    write SetMinimize;
      [TXMLSerial] property Rect;
      [TXMLSerial] property Name;
      [TXMLSerial] property Color;
      [TXMLSerial] property Hide;
      [TXMLSerial] property Enable;
      [TXMLSerial] property TextureName;
      [TXMLSerial] property Font;
      [TXMLSerial] property Parent;
      [TXMLSerial] property Hint;
      [TXMLSerial] property Blend;
    public
      property ComponentName[index: string]: TGUIObject read GetComponentByStr;
      property Component[index: integer]: TGUIObject    read GetComponentByIndex;

      property ButtonMinimize: TGUIButton               read FBtnMinimize;
      property ButtonHide: TGUIButton                   read FBtnHide;
    public
      procedure AfterObjRender; override;
      procedure Render; override;
      procedure RenderText; override;
      procedure RenderStatusText;
  end;

implementation

{ TGUIForm }

procedure TGUIForm.ShowHint(AComp: TGUIObject; AmP: TCoord2DI);
begin
  if AComp.Hint.Text = '' then
  begin
    FHint.Hide:= True;
    Exit;
  end;

  FHint.Color     := AComp.Hint.BackgroundColor;
  FHint.Text      := AComp.Hint.Text;
  FHint.Font.Color:= AComp.Hint.Color;
  FHint.Rect.SetPos(AmP.X + MOUSE_AREA, Amp.Y + MOUSE_AREA);
  FHint.SetViewPort(FViewPort[2] - Rect.X, FViewPort[3] - Rect.Y - FOffsetY);
  FHint.Hide := False;
end;

procedure TGUIForm.MakeButton(var pButton: TGUIButton; pProc: TGUIProc; pImage, pImageDown: Integer);
begin
  pButton:= TGUIButton.Create('', Self.GetTextureLink);
  pButton.Area.Show:= True;
  pButton.OnClick:= pProc;
  pButton.VertexList.SetVertexTextureMap( 4, GUIPalette.GetCellRect(pImage));
  pButton.VertexList.SetVertexTextureMap(12, GUIPalette.GetCellRect(pImageDown));
end;

function TGUIForm.AddComponent(pComponent: TGUIObject): Boolean;
var Msg: TGUIMessage;
begin
  Result:= False;
  if not Assigned(pComponent) then
    Exit;

  try
    //Добавим текстуру от формы если нет у компонента
    if GetTextureLink <> nil then
      if pComponent.GetTextureLink = nil then
        pComponent.SetTextureLink(GetTextureLink);

    //Добавим шрифт от формы если нет у компонента
    if Font.GetTextureLink <> nil then
      if pComponent.Font.GetTextureLink = nil then
        pComponent.Font.CopyFrom(Font);

    inc(FIndex);
    pComponent.GUID  := TGUID.NewGuid.ToString;
    pComponent.Parent:= Self;
    //Пост инициализация
    pComponent.ProcessEvents;

    //Отправим всей форме что новый компонент добавлен
    BroadcastMessage(Msg.Make(MSG_FORM_ADDCOMPONENT, pComponent));

    //Добавим компонент в список
    FComponent.Add(pComponent);
    Result:= True;
  finally
    FProps:= fpNeedRecalc;
    //Тут ничего не делаем
  end;

end;

procedure TGUIForm.AfterObjRender;
var i: integer;
    Comp: TGUIObject;
    FormRect: TGUIObjectRect;
begin
  if not IsAssigned then
    Exit;

  RenderStatusText;
  GetCurrentViewPort;

  glPushMatrix;
  FormRect:= GetRectForm;
  glScissor(FormRect.X, FViewPort[3] - FormRect.Height - FormRect.Y + 1, FormRect.Width, FormRect.Height); // От левого нижнего угла
  glTranslatef(0, FOffsetY, 0);

  if Assigned(FComponent) then
    for i := 0 to FComponent.Count - 1 do
    begin
      glEnable(GL_SCISSOR_TEST);
      Comp:= TGUIObject(FComponent[i]);

      //Компонент с отложенной отрисовкой
      if FFocused.IsAssigned and (Comp = FFocused.Comp) and
         (rpRenderLast in Comp.RenderProps) then
          Continue;

      Comp.Render;
    end;

  //Если установлен флаг отложенной прорисовки (например для комбобокса)
  if FFocused.IsAssigned then
    if rpRenderLast in FFocused.Comp.RenderProps then
      FFocused.Comp.Render;

  glDisable(GL_SCISSOR_TEST);
  glPopMatrix;

  //Отображаем PopupMenu
  if Assigned(FActivePopup) then
    FActivePopup.Render;

  //Отображаем всплывающую подсказку
  if Assigned(FHint) then
    FHint.Render;
end;

procedure TGUIForm.BroadcastMessage(pMessage: TGUIMessage);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).BroadcastMessage(pMessage);
end;

constructor TGUIForm.Create(pName: String; pTextureLink: TTextureLink);
var ARect: TGUIObjectRect;
begin
  inherited Create(pName, gtcForm);
  FComponent:= TList.Create;
  FHint:= TGUIFormHint.Create;
  FFocused:= TGUIFocusComp.Create;

  //Кнопки на заголовке
  MakeButton(FBtnMinimize, OnMinimize, pal_BtnMinimizeUp, pal_BtnMinimizeDn);
  MakeButton(FBtnHide    , OnHide    , pal_BtnCloseUp   , pal_BtnCloseDn);

  FProps:= fpNone;
  FStatus.Clear;
  FMinWidth := 100;
  FMinHeight:= 100;
  FPrevRect.SetRect(0, 0, 200, 100);
  SetRect(FPrevRect);
  FLastPosX := 0;
  FLastPosY := 0;
  FStatus.OffsetX:= 3;
  SetTextureLink(pTextureLink);
  SetBorderStyle(fsSingle);

  ARect:= GetRectForm;
  VertexList.MakeSquare(ARect, Color, GUIPalette.GetCellRect(pal_Frame), GROUP_FORM);
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_Window), GROUP_FORM);

  //Заголовок
  ARect:= GetRectCaption;
  VertexList.MakeSquare(ARect, clWhite, clWhite, clGray, clGray, GUIPalette.GetCellRect(pal_Frame), GROUP_CAPTION);
  VertexList.MakeSquareOffset(8, 1, clGray, clWhite, clWhite, clGray, GUIPalette.GetCellRect(pal_Window), GROUP_CAPTION);

  //Статус бар
  ARect:= GetRectStatus;
  VertexList.MakeSquare(ARect, clGray, GUIPalette.GetCellRect(pal_2), GROUP_STATUS_BAR);
  VertexList.MakeSquareOffset(16, 1, clGray, GUIPalette.GetCellRect(pal_Window), GROUP_STATUS_BAR);

  ARect:= GetRectStatusBtn;
  VertexList.MakeSquare(ARect, clGray, GUIPalette.GetCellRect(pal_16), GROUP_STATUS_BTN);
end;

function TGUIForm.CurrOnHit(pRect: TGUIObjectRect; pX, pY: Integer): Boolean;
begin
  Result:= (pX >= pRect.X) and (pX <= pRect.X + pRect.Width) and
           (pY >= pRect.Y) and (pY <= pRect.Y + pRect.Height);
end;

function TGUIForm.DelComponent(pComponent: TGUIObject): Boolean;
var i: integer;
begin
  Result:= False;

  i:= FindByGUID(pComponent);
  if not IndexExists(i) then
    Exit;

  try
    TGUIObject(FComponent[i]).Free;
    FComponent[i]:= nil;
    Result:= True;
  finally
    FProps:= fpNeedRecalc;
    FComponent.Pack;
  end;

end;

destructor TGUIForm.Destroy;
var i: integer;
begin
  if Assigned(FComponent) then
    for i := 0 to FComponent.Count - 1 do
      TGUIObject(FComponent[i]).Free;

  FreeAndNil(FPopupMenu);
  FreeAndNil(FFocused);
  FreeAndNil(FBtnMinimize);
  FreeAndNil(FBtnHide);
  FreeAndNil(FComponent);
  FreeAndNil(FHint);
  inherited;
end;

procedure TGUIForm.DestroyActivePopup;
begin
  if Assigned(FActivePopup) then
    FActivePopup.Hide:= True;

  FActivePopup:= nil;
end;

function TGUIForm.FindByGUID(pComponent: TGUIObject): Integer;
var i: integer;
begin
  Result:= -1;
  for i := 0 to FComponent.Count - 1 do
    if SameText(pComponent.GUID, TGUIObject(FComponent[i]).GUID) then
      Exit(i);
end;

procedure TGUIForm.FreeActivePopup;
begin
  if Assigned(FActivePopup) then
    FActivePopup.Hide:= True;

  FActivePopup:= nil;
end;

function TGUIForm.GetComponentByIndex(Index: Integer): TGUIObject;
begin
  Result:= nil;
  if not IndexExists(Index) then
    Exit;

  Result:= TGUIObject(FComponent[Index]);
end;

function TGUIForm.GetComponentByStr(Index: String): TGUIObject;
var i: integer;
begin
  Result:= nil;

  i:= Search(Index);
  if not IndexExists(i) then
    Exit;

  Result:= TGUIObject(FComponent[i]);
end;

function TGUIForm.GetCorrPos(pX, pY: Integer): TCoord2DI;
begin
  Result.X:= pX - Rect.X;
  Result.Y:= pY - Rect.Y - FOffsetY;
end;

procedure TGUIForm.GetCurrentViewPort;
begin
  glGetIntegerv(GL_VIEWPORT, @FViewPort);
end;

function TGUIForm.GetRectBtnHide: TGUIObjectRect;
begin
  Result.SetRect(Rect.X + Rect.Width - BUTTON_SIZE - 2, Rect.Y + 2, BUTTON_SIZE, BUTTON_SIZE);
end;

function TGUIForm.GetRectBtnMin: TGUIObjectRect;
var LeftOffset: Integer;
begin
  LeftOffset:= 0;
  if Assigned(FBtnHide) and
    (not FBtnHide.Hide) then
     LeftOffset:= BUTTON_SIZE;

  Result.SetRect(Rect.X + Rect.Width - BUTTON_SIZE - 2 - LeftOffset, Rect.Y + 2, BUTTON_SIZE, BUTTON_SIZE);
end;

function TGUIForm.GetRectCaption: TGUIObjectRect;
begin
  Result.SetRect(0, 0, 0, 0);

  if FBorderStyle = fsNone then
    Exit;

  Result.SetRect(
     Rect.X, Rect.Y,
     Rect.Width, CAPTION_HEIGHT
   );
end;

function TGUIForm.GetRectForm: TGUIObjectRect;
begin
  Result.SetRect(Rect);
  case FBorderStyle of
    //default fsNone    : Result.SetRect(Rect);
    fsSingle  : Result.SetRect(Rect.X, Rect.Y + CAPTION_HEIGHT, Rect.Width, Rect.Height);
    fsSizeable: Result.SetRect(Rect.X, Rect.Y + CAPTION_HEIGHT, Rect.Width, Rect.Height - STATUS_HEIGHT - CAPTION_HEIGHT);
  end;
end;

function TGUIForm.GetRectStatus: TGUIObjectRect;
begin
  Result.SetRect(0, 0, 0, 0);

  if FBorderStyle <> fsSizeable then
    Exit;

  Result.SetRect(
     Rect.X, Rect.Y + Rect.Height - STATUS_HEIGHT,
     Rect.Width - STATUS_BTN_SIZE, STATUS_HEIGHT
   );
end;

function TGUIForm.GetRectStatusBtn: TGUIObjectRect;
begin
  Result.SetRect(0, 0, 0, 0);

  if FBorderStyle <> fsSizeable then
    Exit;

  Result.SetRect(
     Rect.X + Rect.Width - STATUS_BTN_SIZE, Rect.Y + Rect.Height - STATUS_HEIGHT,
     STATUS_BTN_SIZE, STATUS_HEIGHT
   );
end;

function TGUIForm.HasActivePopup: Boolean;
begin
  Result:= (Assigned(FActivePopup) and (not FActivePopup.Hide));
end;

function TGUIForm.IndexExists(pIndex: integer): Boolean;
begin
  Result:= (Assigned(FComponent) and (pIndex > -1) and (pIndex < FComponent.Count)
    and (FComponent[pIndex] <> nil));
end;

function TGUIForm.IsAssigned: Boolean;
begin
  Result:= (not Hide) and (Enable) and (not Minimize);
end;

function TGUIForm.OnClose: Boolean;
begin
  Result:= True;
end;

procedure TGUIForm.OnDeactivateForm;
var i: Integer;
    Comp: TGUIObject;
begin
  if Assigned(FComponent) then
    for i := 0 to FComponent.Count - 1 do
    begin
      Comp:= TGUIObject(FComponent[i]);
      Comp.OnDeactivate(Self);
    end;

  FHint.Hide:= True;
  DestroyActivePopup;
end;

procedure TGUIForm.OnHide(Sender: TObject; ParamObj: Pointer);
begin
  Self.Hide:= True;
end;

function TGUIForm.OnHit(pX, pY: Integer): Boolean;
begin
  Result:= False;
  FDownAction:= aiNone;

  //Если окно скрыто
  if FMinimize then
  begin
    if CurrOnHit(GetRectCaption, pX, pY) then
    begin
      FDownAction:= aiCaption;
      Result:= True;
    end;

    Exit;
  end;

  if CurrOnHit(GetRectForm, pX, pY) then
    FDownAction:= aiForm
  else
  if CurrOnHit(GetRectCaption, pX, pY) then
    FDownAction:= aiCaption
  else
  if CurrOnHit(GetRectStatusBtn, pX, pY) then
    FDownAction:= aiStatusBtn
  else
  if CurrOnHit(GetRectStatus, pX, pY) then
    FDownAction:= aiStatus;

  if HasActivePopup then
    if FActivePopup.OnHit(pX - Rect.X, pY - Rect.Y) then
       FDownAction:= aiPopup;

  Result:= FDownAction <> aiNone;
end;

function TGUIForm.OnHitByComponent(pX, pY: Integer): Boolean;
var mP: TCoord2DI;
begin
  Result:= OnHit(pX, pY);
  if Result then
    Exit;

  if not FFocused.IsAssigned then
    Exit;

  mP:= GetCorrPos(pX, pY);
  Result:= FFocused.Comp.OnHit(mP.X, mP.Y);

  if Result then
    Exit;

  if not HasActivePopup then
    Exit;

  Result:= FActivePopup.OnHit(pX, pY);

end;

procedure TGUIForm.OnKeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FFocused.IsAssigned then
    Exit;

  FFocused.Comp.OnKeyDown(Key, Shift);
end;

procedure TGUIForm.OnKeyPress(Key: Char);
begin
  if not FFocused.IsAssigned then
    Exit;

  FFocused.Comp.OnKeyPress(Key);
end;

procedure TGUIForm.OnKeyUp(var Key: Word; Shift: TShiftState);
begin
  if not FFocused.IsAssigned then
    Exit;

  FFocused.Comp.OnKeyUp(Key, Shift);
end;

procedure TGUIForm.OnMinimize(Sender: TObject; ParamObj: Pointer);
begin
  Minimize:= not Minimize;
end;

procedure TGUIForm.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);

function FocusedMouseDown(pCx, pCy: Integer; pButton: TGUIMouseButton): Boolean;
begin
  Result:= False;
  if not FFocused.IsAssigned then
    Exit;

  //Не попали по форме (проверим на skipscissor)
  if FDownAction <> aiForm then
    if not (rpSkipScissor in FFocused.Comp.RenderProps) then
      Exit;

  if not FFocused.Comp.OnHit(pCx, pCy) then
  begin
    FFocused.Comp.OutHit(pCx, pCy);
    Exit;
  end;

  FFocused.Comp.BeforeOnMouseDown(pCx, pCy, pButton);

  if not Assigned(FActivePopup) then
    FActivePopup:= TGUIPopupMenu(FFocused.Comp.GetChildItemPopup);

  if not ShowPopupMenu(FActivePopup, pCx, pCy, pButton) then
    FFocused.Comp.OnMouseDown(pCx, pCy, pButton)
  else
    FreeActivePopup;

  if goaDown in FFocused.Comp.GetAction then
    Result:= True;
end;

var i: integer;
    Comp: TGUIObject;
    FProc: Boolean;
    mP: TCoord2DI;
begin

  //Для заполнения FDownAction
  OnHit(pX, pY);

  mP:= GetCorrPos(pX, pY);

  //Нажали на Popup
  if FDownAction = aiPopup then
  begin
    ShowPopupMenu(FActivePopup, mP.X, mP.Y + FOffsetY, Button);
    Exit;
  end;

  FProc:= FocusedMouseDown(mP.X, mP.Y, Button);

  case FDownAction of
    aiNone: begin
//      FocusedMouseDown(Cx, Cy, Button);
    end;

    //Нажали в область формы
    aiForm: begin
      if FProc then
        Exit;

      FreeActivePopup;
      FFocused.Comp:= nil;

      if Assigned(FComponent) then
        for i := 0 to FComponent.Count - 1 do
        begin
          Comp:= TGUIObject(FComponent[i]);

          //Компонент скрыт
          if Comp.Hide then
            Continue;

          //Если нажали не на компонент
          if not Comp.OnHit(mP.X, mP.Y) then
          begin
            Comp.OutHit(mP.X, mP.Y);
            Continue;
          end;

          //Передадим на событие до появления меню например для изменения имени пункта меню
          Comp.BeforeOnMouseDown(mP.X, mP.Y, Button);

          //Если было до этого раскрыто какое то PopupMenu то скроем его
          FreeActivePopup;

          //Проверяем у компонента Popup меню
          FActivePopup:= TGUIPopupMenu(Comp.PopupMenu);

          //Получить какой то другой активный popup например у ListBox
          if not Assigned(FActivePopup) then
            FActivePopup:= TGUIPopupMenu(Comp.GetChildItemPopup);

          Comp.OnMouseDown(mP.X, mP.Y, Button);
          FFocused.Comp:= Comp;
        end;

      ShowPopupMenu(FActivePopup, mP.X, mP.Y + FOffsetY, Button);

    end;

    //Нажали на заголовок
    aiCaption: begin
      FLastPosX:= pX;
      FLastPosY:= pY;

      if Assigned(FBtnMinimize) then
        FBtnMinimize.OnMouseDown(pX, pY, Button);

      if Assigned(FBtnHide) then
        FBtnHide.OnMouseDown(pX, pY, Button);
    end;

    //Нажали на статус бар
    aiStatus: begin

    end;

    aiStatusBtn: begin
      //
      VertexList.SetGroupColor(GROUP_STATUS_BTN, clWhite);
    end;
  end;

end;

procedure TGUIForm.OnMouseMove(pX, pY: Integer);
var i          : integer;
    Comp       : TGUIObject;
    mP         : TCoord2DI;
    ObjHit     : Boolean;
    MouseOnForm: Boolean;
begin
  //Скрываем хинт если показан
  FHint.Hide:= True;
  ObjHit:= False;

  if Hide then
    Exit;

  if Assigned(FBtnHide) then
  begin
    FBtnHide.OnMouseMove(pX, pY);
    if FBtnHide.ObjectInAction([goaDown]) then
      Exit;
  end;

  if Assigned(FBtnMinimize) then
  begin
    FBtnMinimize.OnMouseMove(pX, pY);
    if FBtnMinimize.ObjectInAction([goaDown]) then
      Exit;
  end;

  if HasActivePopup then
  begin
    FActivePopup.OnMouseMove(pX - Rect.X, pY - Rect.Y);
    Exit;
  end;

  case FDownAction of
    aiCaption  : begin
      SetPos(Rect.X + (pX - FLastPosX), Rect.Y + (pY - FLastPosY));
      FLastPosX:= pX;
      FLastPosY:= pY;
      SetPosButtons;
      Exit;
    end;

    aiStatusBtn: begin
      SetResizeForm(pX + 2, pY + 2);
      SetPosButtons;
      Exit;
    end;
  end;

  if Hide or Minimize then
    Exit;

  mP:= GetCorrPos(pX, pY);
  MouseOnForm:= CurrOnHit(GetRectForm, pX, pY);

  //Для элемента в фокусе всегда OnMouseMove (для TList) например
  //Если не скипается обрезка по форме
  if FFocused.IsAssigned then
    if (MouseOnForm) or
       (rpSkipScissor in FFocused.Comp.RenderProps) then
    begin
      FFocused.Comp.OnMouseMove(mP.X, mP.Y);

      if MouseOnForm then
        ShowHint(FFocused.Comp, mP);

      //Exit тут нельзя (нужно события отправить остальным компонентам)
      if FFocused.Comp.OnHit(mP.X, mP.Y) then
        mP.SetDefault;
    end;

  //Когда изменяем размер окна
  if not MouseOnForm then
    mP.SetDefault;

  for i:= 0 to FComponent.Count - 1 do
  begin
    Comp:= TGUIObject(FComponent.Items[i]);

    if Comp = FFocused.Comp then
      Continue;

    Comp.OnMouseMove(mP.X, mP.Y);

    if not Comp.Hint.Enable then
      Continue;

    if ObjHit then
      Continue;

    //Всплывающая подсказка
    if not Assigned(FHint) then
      Continue;

    //Уже показана подсказка
    if not FHint.Hide then
      Continue;

    //
    if not Comp.OnHit(mP.X, mP.Y) then
      Continue;

    ObjHit:= True;

    //Показываем Hint
    ShowHint(Comp, mP);
  end;
end;

procedure TGUIForm.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);

  function FocusedMouseUp(pCx, pCy: Integer; pButton: TGUIMouseButton): Boolean;
  begin
    Result:= False;
    if not FFocused.IsAssigned then
      Exit;

    FFocused.Comp.OnMouseUp(pCx, pCy, pButton);
    if not (goaDown in FFocused.Comp.GetAction) then
      Result:= True;
  end;

var mP: TCoord2DI;
begin
  inherited;

  mP:= GetCorrPos(pX, pY);
  FocusedMouseUp(mP.X, mP.Y, Button);

  case FDownAction of
    aiPopup: begin
      if HasActivePopup then
        FActivePopup.OnMouseUp(mP.X, mP.Y + FOffsetY, Button);
    end;

    aiNone: begin
      //
    end;

    aiCaption: begin
      if Assigned(FBtnMinimize) then
        FBtnMinimize.OnMouseUp(pX, pY, Button);

      if Assigned(FBtnHide) then
        FBtnHide.OnMouseUp(pX, pY, Button);
    end;

    aiForm: begin
     //
    end;

    aiStatusBtn: begin
      VertexList.SetGroupColor(GROUP_STATUS_BTN, clGray);
    end;
  end;

  FDownAction:= aiNone;
end;

procedure TGUIForm.OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer);
begin
  if not FFocused.IsAssigned then
    Exit;

  FFocused.Comp.OnMouseWheelDown(Shift, MPosX, MPosY);
end;

procedure TGUIForm.OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer);
begin
  if not FFocused.IsAssigned then
    Exit;

  FFocused.Comp.OnMouseWheelUp(Shift, MPosX, MPosY);
end;

procedure TGUIForm.OutHit(pX, pY: Integer);
begin
  inherited;

end;

procedure TGUIForm.ProcessEvents;
begin
  FProps:= fpNone;
  SetResize;
end;

procedure TGUIForm.Render;
begin
  if Hide then
    Exit;

  if FProps = fpNeedRecalc then
    ProcessEvents;

  inherited;

  if FBorderStyle = fsNone then
    Exit;

  if Assigned(FBtnHide) then
    FBtnHide.Render;

  if Assigned(FBtnMinimize) then
    FBtnMinimize.Render;
end;

procedure TGUIForm.RenderStatusText;
var ARect: TGUIObjectRect;
begin
  if FBorderStyle <> fsSizeable then
    Exit;

  if FMinimize then
    Exit;

  ARect:= GetRectStatus;
  FFont.RenderText(ARect.X - Rect.X + FStatus.OffsetX, ARect.Y - Rect.Y + FStatus.OffsetY, FStatus.Text, ARect.Width);
end;

procedure TGUIForm.RenderText;
begin
  //Не отображаем текст заголовка
  if FBorderStyle = fsNone then
    Exit;

  inherited;
  FFont.RenderText(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FCaption, Rect.Width);
end;

function TGUIForm.Search(pName: String): Integer;
var i: integer;
begin
  Result:= -1;

  if not Assigned(FComponent) then
    Exit;

  for i := 0 to FComponent.Count - 1 do
    if SameText(TGUIObject(FComponent[i]).Name, pName) then
      Exit(i);
end;

procedure TGUIForm.SetBorderStyle(pValue: TGUIFormBorderStyle);
var HideCaption: Boolean;
    HideStatus : Boolean;
begin
  HideCaption := False;
  HideStatus  := False;

  FBorderStyle:= pValue;
  FOffsetY    := 0;

  case FBorderStyle of
    fsNone    : begin
                  HideCaption:= True;
                  HideStatus := True;
                end;
    fsSingle  : begin
                  FOffsetY:= CAPTION_HEIGHT;
                  HideStatus := True;
                end;
    fsSizeable: begin
                  FOffsetY:= CAPTION_HEIGHT;
                end;
  end;

  FBtnMinimize.Hide:= FBorderStyle = fsNone;
  FBtnHide.Hide    := FBorderStyle = fsNone;

  VertexList.SetGroupHide(GROUP_CAPTION   , HideCaption);
  VertexList.SetGroupHide(GROUP_STATUS_BAR, HideStatus);
  SetResize;
end;

procedure TGUIForm.SetCenterByRect(pRect: TRect);
begin
  X:= Round((pRect.Width / 2) - (Width / 2));
  Y:= Round((pRect.Height / 2) - (Height / 2));

  SetResize;
end;

procedure TGUIForm.SetFontEvent;
begin
  inherited;

  if fsetTexture in Font._Setter then
    FHint.Font.SetTextureLink(Self.Font.GetTextureLink);

  FTextOffset.X:= 2;
  FTextOffset.Y:= Round((CAPTION_HEIGHT / 2) - (Font.Height / 2)) - 1;
end;

procedure TGUIForm.SetMinimize(pValue: Boolean);
begin
  if FBorderStyle = fsNone then
  begin
    FMinimize:= False;
    Exit;
  end;

  if pValue = FMinimize then
    Exit;

  FMinimize:= pValue;

  VertexList.SetGroupHide(GROUP_FORM      , FMinimize);
  VertexList.SetGroupHide(GROUP_STATUS_BAR, FMinimize);
  VertexList.SetGroupHide(GROUP_STATUS_BTN, FMinimize);
end;

procedure TGUIForm.SetPosButtons;
begin
  if Assigned(FBtnMinimize) then
    FBtnMinimize.SetRect(GetRectBtnMin);

  if Assigned(FBtnHide) then
    FBtnHide.SetRect(GetRectBtnHide);
end;

procedure TGUIForm.ComponentAlignment(AOffsetRect: TGUIObjectRect);
var ARect: TGUIObjectRect;
    RAlg : TGUIObjectRect;
    i    : integer;
    Comp : TGUIObject;
begin
  ARect:= GetRectForm;
  if not Assigned(FComponent) then
    Exit;

  RAlg.SetRect(0, 0, 0, 0);
  for i := 0 to FComponent.Count - 1 do
  begin
    Comp:= TGUIObject(FComponent[i]);
    Comp.RemoveAction([goaAlignRect]);

    case Comp.Align of
      alCustom: begin
        if not (anLeft in Comp.Anchors) then
        begin
          Comp.X:= Comp.X + AOffsetRect.Width;
          Comp.SetAction([goaAlignRect]);
        end;

        if not (anTop in Comp.Anchors) then
        begin
          Comp.Y:= Comp.Y + AOffsetRect.Height;
          Comp.SetAction([goaAlignRect]);
        end;

        if (anRight in Comp.Anchors) then
        begin
          Comp.Rect.SetAnchWidth(Comp.Width + AOffsetRect.Width);
          Comp.SetAction([goaAlignRect]);
        end;

        if (anBottom in Comp.Anchors) then
        begin
          Comp.Rect.SetAnchHeight(Comp.Height + AOffsetRect.Height);
          Comp.SetAction([goaAlignRect]);
        end;
      end;

      //Верх
      alTop: begin
        Comp.X:= 1;
        Comp.Y:= RAlg.Y;
        Comp.Rect.SetAnchWidth(ARect.Width - 2);
        Comp.Rect.SetAnchHeight(Comp.Height);
        RAlg.Y:= RAlg.Y + Comp.Height;
        Comp.SetAction([goaAlignRect]);
      end;

      alBottom: begin
        RAlg.Height:= RAlg.Height + Comp.Height;
        Comp.X:= 1;
        Comp.Y:= ARect.Height - RAlg.Height - 2;
        Comp.Rect.SetAnchWidth(ARect.Width - RAlg.X - 2);
        Comp.SetAction([goaAlignRect]);
      end;
    end;
  end;

  for i := 0 to FComponent.Count - 1 do
  begin
    Comp:= TGUIObject(FComponent[i]);

    case Comp.Align of
      alLeft  : begin
        Comp.X:= RAlg.X;
        Comp.Y:= RAlg.Y;
        Comp.Rect.SetAnchHeight(ARect.Height - RAlg.Height - RAlg.Y - 2);
        RAlg.X:= RAlg.X + Comp.Width;
        Comp.SetAction([goaAlignRect]);
      end;

      alRight: begin
        RAlg.Width:= RAlg.Width + Comp.Width;
        Comp.X:= ARect.Width - RAlg.Width - 2;
        Comp.Y:= RAlg.Y;
        Comp.Rect.SetAnchHeight(ARect.Height - RAlg.Height - RAlg.Y - 2);
        Comp.SetAction([goaAlignRect]);
      end;
    end;

  end;

  for i := 0 to FComponent.Count - 1 do
  begin
    Comp:= TGUIObject(FComponent[i]);

    if Comp.Align <> alClient then
    begin
      if goaUpdateSize in Comp.GetAction then
        Comp.OnResize;

      Comp.RemoveAction([goaAlignRect]);
      Continue;
    end;

    Comp.SetAction([goaAlignRect]);
    Comp.X:= RAlg.X + 1;
    Comp.Y:= RAlg.Y + 1;
    Comp.Rect.SetAnchWidth(ARect.Width - RAlg.X - RAlg.Width - 2);
    Comp.Rect.SetAnchHeight(ARect.Height - RAlg.Y - RAlg.Height - 3);
    Comp.OnResize;
    Comp.RemoveAction([goaAlignRect]);
  end;

end;

procedure TGUIForm.SetResize;
var ARect: TGUIObjectRect;
    ROff: TGUIObjectRect;
begin

  ARect:= GetRectForm;

  ROff.X     := ARect.X      - FPrevRect.X;
  ROff.Y     := ARect.Y      - FPrevRect.Y;
  ROff.Width := ARect.Width  - FPrevRect.Width;
  ROff.Height:= ARect.Height - FPrevRect.Height;

  VertexList.SetCalcSquare(0, Rect, ARect, 0);
  VertexList.SetCalcSquare(4, Rect, ARect, 1);

  ARect:= GetRectCaption;
  VertexList.SetCalcSquare(8 , Rect, ARect, 0);
  VertexList.SetCalcSquare(12, Rect, ARect, 1);

  ARect:= GetRectStatus;
  VertexList.SetCalcSquare(16, Rect, ARect, 0);
  VertexList.SetCalcSquare(20, Rect, ARect, 1);

  ARect:= GetRectStatusBtn;
  VertexList.SetCalcSquare(24, Rect, ARect, 0);

  FPrevRect:= GetRectForm;
  SetPosButtons;
  ComponentAlignment(ROff);
end;

procedure TGUIForm.SetResizeForm(pX, pY: Integer);
var Ax, Ay: Integer;
begin
  Ax:= pX - Rect.X;
  Ay:= pY - Rect.Y;

  if FMinWidth > Ax then
    Ax:= FMinWidth;

  if FMinHeight > Ay then
    Ay:= FMinHeight;

  SetSize(Ax, Ay);
end;

procedure TGUIForm.SetTextureLink(pTextureLink: TTextureLink);
begin
  inherited;

  if Assigned(FBtnMinimize) then
    FBtnMinimize.SetTextureLink(pTextureLink);
  if Assigned(FBtnHide) then
    FBtnHide.SetTextureLink(pTextureLink);
end;

function TGUIForm.ShowPopupMenu(pPopupMenu: TGUIObject; pX, pY: Integer; pButton: TGUIMouseButton): Boolean;
begin
  Result:= False;

  if not Assigned(pPopupMenu) then
  begin
    //Если нажали на форму, и на компонентах не было popupmenu
    if not FFocused.IsAssigned then
      FActivePopup:= TGUIPopupMenu( FPopupMenu );

    if not Assigned(FActivePopup) then
      Exit;
  end
  else
    FActivePopup:= TGUIPopupMenu( pPopupMenu );

  FActivePopup.OnMouseDown(pX, pY, pButton);

  Result:= not FActivePopup.Hide;
end;

{ TGUIFormStatus }

procedure TGUIFormStatus.Clear;
begin
  Text   := '';
  OffsetX:= 0;
  OffsetY:= 0;
end;

{ TGUIFormHint }

constructor TGUIFormHint.Create(pTextureFont: TTextureLink);
begin
  inherited Create('Hint', gtcObject);

  Text:= '';
  FFont.SetTextureLink(pTextureFont);
  SetTextureLink(nil);

  FTextOffset.X:= 2;
  FTextOffset.Y:= 2;

  //Параметры цвета передаются от TGUIHintObject
  //тут их нет смысла устанавливать

  SetRect(0, 0, 1, 1);

  VertexList.MakeSquare(Rect, 0, nil, 0);
  VertexList.MakeSquare(Rect, Color, nil, 1);
end;

procedure TGUIFormHint.Render;
begin
  if Hide then
    Exit;

  if GetCurrentTime - FLastTime < FWaitStart then
    Exit;

  if GetCurrentTime - FLastTime > FShowTime  then
    Exit;

  inherited;

  FFont.RenderText(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FText, 0, True);
end;

procedure TGUIFormHint.SetColor(pColor: TColor);
begin
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
    FShowTime := 5000;
  end;
end;

procedure TGUIFormHint.SetText(pText: String);
var outWidth, outHeight: Integer;
begin
  if not SameText(FText, pText) then
  begin
    FFont.GetTextRect(pText, outWidth, outHeight);
    Rect.SetSize(
       outWidth  + (FTextOffset.X * 2),
       outHeight + (FTextOffset.Y * 4)
    );

    //Размеры фона
    VertexList.SetSizeSquare(0, Rect);
    VertexList.SetSizeSquare(4, Rect, 1);
  end;

  FText:= pText;
end;

procedure TGUIFormHint.SetViewPort(pWidth, pHeight: Integer);
var nX, nY: Integer;
begin
  nX:= Rect.X;
  nY:= Rect.Y;

  if Rect.X + Rect.Width > pWidth then
    nX:= Rect.X - Rect.Width - MOUSE_AREA - 5;

  if Rect.Y + Rect.Height > pHeight then
    nY:= Rect.Y - Rect.Height;

  if (Rect.X = nX) and (Rect.Y = nY) then
    Exit;

  Rect.SetPos(nX, nY);
  VertexList.SetSizeSquare(0, Rect);
  VertexList.SetSizeSquare(4, Rect, 1);
end;

{ TGUIFocusComp }

constructor TGUIFocusComp.Create;
begin
  FComp:= nil;
end;

destructor TGUIFocusComp.Destroy;
begin
  //Не надо тут удалять FComp
  //Это сделает форма
  inherited;
end;

function TGUIFocusComp.IsAssigned: Boolean;
begin
  Result:= Assigned(FComp);
end;

procedure TGUIFocusComp.Render;
begin
  if Assigned(FComp) then
    FComp.Render;
end;

procedure TGUIFocusComp.SetComponent(AObj: TGUIObject);
begin
  FComp:= AObj;
end;

end.
