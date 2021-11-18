unit dlGUIObject;

interface

uses RTTI, Classes, Graphics, SysUtils, dlOpenGL, dlGUITypes, dlGUIFont, dlGUIVertexController, TypInfo,
  dlGUIXmlSerial;

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

//Сообщения GUI компонентам
 const MSG_FORM_ADDCOMPONENT = 1; //
       MSG_CHNG_RADIOBUTTON  = 2; //Изменилось состояние RadioButton

 type
   //Компоненты
   TGUITypeDefName = record
     Name: String; //Название компонента по умолчанию
   end;

   TGUITypeComponent = (gtcObject, gtcForm, gtcButton, gtcPopupMenu, gtcCheckBox, gtcLabel, gtcImage, gtcProgressBar,
     gtcEditBox, gtcTrackBar, gtcListBox, gtcRadioButton, gtcBevel, gtcComboBox, gtcPanel, gtcTable, gtcMainMenu);

 //Имена компонентов по умолчанию
 const
   TGUITypeDefNames: array[TGUITypeComponent] of TGUITypeDefName =
     ((Name: 'Object'),
      (Name: 'Form'),
      (Name: 'Button'),
      (Name: 'PopupMenu'),
      (Name: 'CheckBox'),
      (Name: 'Label'),
      (Name: 'Image'),
      (Name: 'ProgressBar'),
      (Name: 'EditBox'),
      (Name: 'TrackBar'),
      (Name: 'ListBox'),
      (Name: 'RadioButton'),
      (Name: 'Bevel'),
      (Name: 'ComboBox'),
      (Name: 'Panel'),
      (Name: 'Table'),
      (Name: 'MainMenu')
     );

type
   TGUIObject = class;
   EGUIObject = class(Exception);

   //Сообщения что то наподобие Windows.TMessage
   TGUIMessage = record
     public
       Msg    : Integer; //Сообщение GUI_WM_USER например
       Sender : TObject; //Текущий объект
     public
       function Make(const AMessage: Integer; ASender: TObject): TGUIMessage;
   end;

   TGUIActionSetter = (
                        goaFocused,             //Фокус на объекте
                        goaDown,                //На объект нажали
                        goaTextureNeedRecalc,   //Нужно пересчитать координаты текстуры
                        goaTextureAlwaysRecalc, //Всегда пересчитываем текстуру
                        goaItemSelect,          //Выбрали элемент (у ListBox например)
                        goaItemClick,           //Нажали на элемент
                        goaWhell,               //Сработала прокрутка
                        goaUpdateSize,          //Поменялись размеры
                        goaAlignRect,           //Изменяем размеры компонента по Align
                        goaKeyDown              //Нажали на кнопку
                      );

   TGUIObjectAction = Set of TGUIActionSetter;
   TGUIObjectAlign = (
     alCustom, //Определено Rect
     alLeft,   //Прилипание к левому краю
     alRight,  //Прилипание к правому краю
     alClient, //Растянуто по всей форме
     alTop,    //Прилипание к верхней части
     alBottom  //Прилипание к нижней части
   );

   //При изменениях размеров формы только в режиме Align = Custom
   TGUIObjectAnchors = (
     anLeft,
     anTop,
     anRight,
     anBottom
   );
   TGUIObjectAnchorsSet = set of TGUIObjectAnchors;

   TGUIRenderProp = (
     rpSkipScissor, //Не обрезать компонент по форме
     rpRenderLast   //Прорисовывать когда компонент активный самым последним
   );
   TGUIRenderProps = set of TGUIRenderProp;

   //Прозрачность объекта
   TGUIObjectAlpha = class(TPersistent)
     public
       FValue : TFloat; //Индекс прозрачности
       FSrc   : TUInt; //Blend src (GL_ONE)
       FDst   : TUInt; //Blend dst (GL_DST_COLOR)
     public
       constructor Create(pValue: TFloat; pSrc, pDst: TUInt);
       procedure SetValue(pValue: TFloat; pSrc: TUInt = GL_ONE; pDst: TUInt = GL_ONE_MINUS_SRC_ALPHA);
     published
       property Value: TFloat read FValue write FValue;
       property Src  : TUInt  read FSrc   write FSrc;
       property Dst  : TUInt  read FDst   write FDst;
   end;

   //Размеры текстуры устанавливаются при назначении текстуры объекту
   TGUITextureInfo = record
     public
       Width : Integer;
       Height: Integer;
     public
       procedure SetSize(pWidth, pHeight: Integer);
   end;

   //Всплывающая подсказка (Параметры потом передаются в класс Hint'а на форме)
   //TGUIFormHint
   TGUIHintObject = class(TPersistent)
     private
       FText   : String;  //Текст всплывающей подсказки
       FColor  : TColor;  //Цвет всплывающей подсказки
       FEnable : Boolean; //Включено отображение подсказки или нет
       FBGColor: TColor;  //Фоновый цвет
     private
       constructor Create;
       procedure SetText(pText: String);
       procedure SetColor(pColor: TColor);
       procedure SetEnable(pEnable: Boolean);
       procedure SetBackgroundColor(pColor: TColor);
     published
       [TXMLSerial] property Text  : String          read FText    write SetText;
       [TXMLSerial] property Color : TColor          read FColor   write SetColor;
       [TXMLSerial] property Enable: Boolean         read FEnable  write SetEnable;
       [TXMLSerial] property BackgroundColor: TColor read FBGColor write SetBackgroundColor;
   end;

   //Объект от которого наследуются компоненты
   TGUIObject = class(TPersistent)
     strict private
       FName        : String; //Название объекта
       FDefName     : String; //Имя данное автоматически (для подставки ID, SetID())
       FType        : TGUITypeComponent; //Тип компонента
     protected
       FGUID        : String; //Номер объекта в листе
       FRect        : TGUIObjectRect; //Позиция и размеры
       FAlign       : TGUIObjectAlign; //Положение на форме
       FAnchors     : TGUIObjectAnchorsSet; //Закрепление позиции на форме (края)
       FTextOffset  : TGUIObjectRect; //Положение текста
       FHide        : Boolean; //Видимость
       FEnable      : Boolean; //Активный компонент или нет
       FAction      : TGUIObjectAction; //Действия над объектом
       FHint        : TGUIHintObject; //Всплывающая подсказка
       FTextureInfo : TGUITextureInfo; //Информация о размерах текстуры
       FShowOnTop   : Boolean; //При фокусе переместить объект в самый конец списка (для ComboBox например)
       FRenderProps : TGUIRenderProps; //Какие то доп опции для рендеринга
     private
       FTextureLink : TTextureLink;
     protected
       FScale       : TFloat; //Увеличение
       FFont        : TGUIFont; //Шрифт
       FBlend       : TBlendParam; //Смешивание цветов
       FColor       : TGLColor; //Цвет
       FModeDraw    : TUInt; //Режим прорисовки
     protected
       FParent      : TGUIObject; //Родитель объекта
       FPopupMenu   : TGUIObject; //Выпадающий список
       FArea        : TGUITypeArea; //Рамка при наведении
     protected
       FVertexList  : TGUIVertexList; //Список вершин
     protected
       //Событие при изменении шрифта
       procedure SetFontEvent; virtual;
       //Установить ссылку на шрифт
       procedure SetFontLink(pFont: TGUIFont); virtual;
       //Установить popupmenu
       procedure SetPopupMenu(pPopupMenu: TGUIObject);

       procedure SetScale(pScale: TFloat); virtual; //Установить размер
       procedure SetWidth(pWidth: Integer); //Установить ширину компонента
       procedure SetHeight(pHeight: Integer); //Установить высоту компонента
       procedure SetResize; virtual; {SetChangeRect} //Вызывается в SetWidth, SetHeight при изменении размера
       procedure SetHide(pHide: Boolean); virtual;
       procedure SetEnable(pEnable: Boolean); virtual;
       procedure SetColor(pColor: TColor); virtual;
       procedure SetAreaResize; virtual;
     strict private
       function GetColor: TColor;
       function GetTextureLinkName: String;
       function GetAttrFocused: Boolean;
       procedure SetGUID(pGUID: String);
       function GetPopupMenuName: String;
       procedure ChangeTextureInfo;
     public
       //Управление FAction
       procedure SetAction(pAction: TGUIObjectAction);
       procedure RemoveAction(pAction: TGUIObjectAction);
       procedure ClearAction;
       function GetAction: TGUIObjectAction;
       function ObjectInAction(pAction: array of TGUIActionSetter; pCheckOr: Boolean = True): Boolean;
     public
       //Узнать позицию родителя (нужно для отрисовки компонента на форме)
       function GetParentPos: TCoord2D;
       //Установить ссылку на текстуру
       procedure SetTextureLink(pTextureLink: TTextureLink); virtual;
       //Скопировать текстуру
       procedure SetTextureCopyFrom(pTextureLink: TTextureLink);
       //Получить ссылку на текущую текстуру
       function GetTextureLink: TTextureLink;
       //Установить позицию компонента
       procedure SetPos(pX, pY: Integer);
       //Установить размеры компонента
       procedure SetSize(pWidth, pHeight: Integer);
       //Установить размеры компонента
       procedure SetRect(pX, pY, pW, pH: Integer); overload;
       procedure SetRect(pRect: TGUIObjectRect); overload;
       //Получить какой то другой активный popup например у ListBox
       function GetChildItemPopup: TGUIObject; virtual;
       //Вызвать SetFontEvent, SetResize
       procedure ProcessEvents;
       procedure OnResize;
     public
       property VertexList   : TGUIVertexList    read FVertexList;
       property GUID         : String            read FGUID               write SetGUID;
       property TextRect     : TGUIObjectRect    read FTextOffset         write FTextOffset;
       property Focused      : Boolean           read GetAttrFocused;
       property ShowOnTop    : Boolean           read FShowOnTop;
       property PopupMenuName: String            read GetPopupMenuName;
       property X            : Integer           read FRect.X             write FRect.X;
       property Y            : Integer           read FRect.Y             write FRect.Y;
       property Width        : Integer           read FRect.Width         write SetWidth;
       property Height       : Integer           read FRect.Height        write SetHeight;

     //[TXMLSerial] ---
     public
       [TXMLSerial] OnClick: TGUIProc; //Нажатие на компонент мышью
     public
       [TXMLSerial] property Name: String read FName;
     public
       property Rect         : TGUIObjectRect       read FRect;
       property ObjectName   : String               read FDefName;
       property ObjectType   : TGUITypeComponent    read FType;
       property Color        : TColor               read GetColor            write SetColor;
       property Hide         : Boolean              read FHide               write SetHide;
       property Enable       : Boolean              read FEnable             write SetEnable;
       property TextureName  : String               read GetTextureLinkName;
       property Font         : TGUIFont             read FFont               write SetFontLink;
       property PopupMenu    : TGUIObject           read FPopupMenu          write SetPopupMenu;
       property Parent       : TGUIObject           read FParent             write FParent;
       property Scale        : TFloat               read FScale              write SetScale;
       property Hint         : TGUIHintObject       read FHint               write FHint;
       property Blend        : TBlendParam          read FBlend              write FBlend;
       property Area         : TGUITypeArea         read FArea               write FArea;
       property Align        : TGUIObjectAlign      read FAlign              write FAlign;
       property Anchors      : TGUIObjectAnchorsSet read FAnchors            write FAnchors;
       property RenderProps  : TGUIRenderProps      read FRenderProps        write FRenderProps;
     public
       constructor Create(pName: String = ''; pType: TGUITypeComponent = gtcObject);
       destructor Destroy; override;

       //Событие нажатия на кнопку клавиатуры
       procedure OnKeyDown(var Key: Word; Shift: TShiftState); virtual;
       //Событие отпускания кнопки клавиатуры
       procedure OnKeyUp(var Key: Word; Shift: TShiftState); virtual;
       //Событие получения символа при нажатии на клавиатуру
       procedure OnKeyPress(Key: Char); virtual;
       //Событие перед нажатием на OnMouseDown например для того чтобы динамически сменить имя PopupMenu
       procedure BeforeOnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); virtual;
       //Событие нажатие на кнопку мыши после BeforeOnMouseDown
       procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); virtual;
       //Событие отпускание кнопки мыши
       procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); virtual;
       //Событие перемещение кнопки мыши
       procedure OnMouseMove(pX, pY: Integer); virtual;
       //Событие двойное нажатие мыши (даблклик)
       procedure OnMouseDoubleClick(pX, pY: Integer; Button: TGUIMouseButton); virtual;
       //Прокрутка колесика мыши вверх
       procedure OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer); virtual;
       //Прокрутка колесика мыши вниз
       procedure OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer); virtual;
       //Проверка входит ли координата pX, pY в область компонента
       function OnHit(pX, pY: Integer): Boolean; virtual;
       //Если не попали в OnHit тогда срабатывает это событие
       procedure OutHit(pX, pY: Integer); virtual;
       //При перемещении мыши у родителя не тестируется OnHit
       procedure OnMouseOver(pX, pY: Integer); virtual;
       //При деактивации "формы"
       procedure OnDeactivate(Sender: TGUIObject); virtual;
     public
       //Прорисовка до Render
       procedure BeforeObjRender; virtual;
       //Прорисовка объекта
       procedure Render; virtual;
       //Прорисовка после Render например при выделении пункта меню
       procedure AfterObjRender; virtual;
       //Прорисовка текста
       procedure RenderText; virtual;
     public
       procedure BroadcastMessage(pMessage: TGUIMessage); virtual;
   end;

implementation

{ TGUIObject }

procedure TGUIObject.ChangeTextureInfo;
begin
  if Assigned(FTextureLink) then
    FTextureInfo.SetSize(FTextureLink.Width, FTextureLink.Height)
  else
    FTextureInfo.SetSize(1, 1);

  //Текстура поменялась или изменился размер, нужно пересчитать
  SetAction([goaTextureNeedRecalc]);
end;

procedure TGUIObject.ClearAction;
begin
  FAction:= [];
end;

constructor TGUIObject.Create(pName: String = ''; pType: TGUITypeComponent = gtcObject);
begin
  FDefName      := TGUITypeDefNames[pType].Name;

  if Trim(pName) <> '' then
    FName:= pName
  else
    FName:= FDefName;

  FType         := pType;
  FGUID         := '';
  FRect.SetRect(0, 0, 0, 0);
  FAlign        := alCustom;
  FAnchors      := [anLeft, anTop];
  FTextOffset.SetRect(0, 0, 0, 0);
  FShowOnTop    := False;
  FHide         := False;
  FEnable       := True;
  FAction       := [];
  FTextureInfo.SetSize(1, 1);
  FVertexList   := TGUIVertexList.Create;
  FTextureLink  := nil;// TTextureLink.Create;
  FFont         := TGUIFont.Create(nil);
  FBlend        := TBlendParam.Create();
  FColor        := TGLColor.Create(clWhite);
  FModeDraw     := GL_QUADS;
  FParent       := nil;
  FPopupMenu    := nil;
  OnClick       := nil;
  FScale        := 1.0;
  FHint         := TGUIHintObject.Create;
  FArea         := TGUITypeArea.Create;
end;

destructor TGUIObject.Destroy;
begin
  FreeAndNil(FPopupMenu);
  FreeAndNil(FVertexList);
  FreeAndNil(FFont);
  FreeAndNil(FBlend);
  FreeAndNil(FColor);
  FreeAndNil(FHint);
  FreeAndNil(FArea);
//  FreeAndNil(FTextureLink); не нужно
end;

procedure TGUIObject.BroadcastMessage(pMessage: TGUIMessage);
begin

end;

function TGUIObject.GetAction: TGUIObjectAction;
begin
  Result:= FAction;
end;

function TGUIObject.GetAttrFocused: Boolean;
begin
  Result:= goaFocused in FAction;
end;

function TGUIObject.GetChildItemPopup: TGUIObject;
begin
  Result:= nil;
end;

function TGUIObject.GetColor: TColor;
begin
  Result:= FColor.GetColor;
end;

function TGUIObject.GetParentPos: TCoord2D;
begin
  Result.X:= 0.0;
  Result.Y:= 0.0;

  if not Assigned(FParent) then
    Exit;

  Result.X:= FParent.FRect.X;
  Result.Y:= FParent.FRect.Y;
end;

function TGUIObject.GetPopupMenuName: String;
begin
  Result:= '';

  if not Assigned(PopupMenu) then
    Exit;

  Result:= PopupMenu.Name;
end;

function TGUIObject.GetTextureLink: TTextureLink;
begin
  Result:= FTextureLink;
end;

function TGUIObject.GetTextureLinkName: String;
begin
  Result:= '';
  if Assigned(FTextureLink) then
    Result:= FTextureLink.Name;
end;

function TGUIObject.ObjectInAction(pAction: array of TGUIActionSetter; pCheckOr: Boolean = True): Boolean;
var i: integer;
begin
  Result:= False;

  if Length(pAction) < 1 then
    Exit;

  for i := 0 to High(pAction) do
    if pCheckOr then
    begin
      if pAction[i] in FAction then
        Exit(True)
    end
    else
     //Если проверяем по And
     if pAction[i] in FAction then
       Result:= True
     else
       Exit(false);
end;

procedure TGUIObject.OnDeactivate(Sender: TGUIObject);
begin
  if Assigned(FArea) then
    FArea.Visible:= False;
end;

function TGUIObject.OnHit(pX, pY: Integer): Boolean;
begin
  Result:= False;

  if Hide then
    Exit;

  Result:=
     (pX >= FRect.X) and
     (pY >= FRect.Y) and
     (pX <= FRect.X + FRect.Width) and
     (pY <= FRect.Y + FRect.Height);
end;

procedure TGUIObject.AfterObjRender;
begin
  if Hide then
    Exit;

  if Assigned(FArea) then
  begin
    SetAreaResize;
    //т.к. у нас уже объект сдвинут glTranslatef то нужно отнять координаты объекта
    //иначе area сдвинется
    FArea.Rect.SetPos( FArea.Rect.X - Rect.X, FArea.Rect.Y - Rect.Y );
    FArea.Render;
  end;
end;

procedure TGUIObject.BeforeObjRender;
begin
  if Hide then Exit;
end;

procedure TGUIObject.BeforeOnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  if Hide then Exit;
end;

procedure TGUIObject.OnKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Hide then Exit;

  SetAction([goaKeyDown]);
end;

procedure TGUIObject.OnKeyPress(Key: Char);
begin
  if Hide then Exit;
end;

procedure TGUIObject.OnKeyUp(var Key: Word; Shift: TShiftState);
begin
  if Hide then Exit;

  RemoveAction([goaKeyDown]);
end;

procedure TGUIObject.OnMouseDoubleClick(pX, pY: Integer; Button: TGUIMouseButton);
begin
  if Hide then Exit;
end;

procedure TGUIObject.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  if Hide then Exit;

  if OnHit(pX, pY) then
    SetAction([goaDown, goaFocused]);
end;

procedure TGUIObject.OnMouseMove(pX, pY: Integer);
begin
  if Hide then
    Exit;

  if Assigned(FArea) then
    if FArea.Show then
      FArea.Visible:= OnHit(pX, pY);
end;

procedure TGUIObject.OnMouseOver(pX, pY: Integer);
begin
  if Hide then
    Exit;

  if Assigned(FArea) then
    FArea.Visible:= False;
end;

procedure TGUIObject.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
begin
  try
    if goaDown in FAction then
      if Assigned(OnClick) then
        if OnHit(pX, pY) and FEnable then OnClick(Self);
  finally
    RemoveAction([goaDown]);
  end;
end;

procedure TGUIObject.OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer);
begin
  if Hide then
    Exit;
end;

procedure TGUIObject.OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer);
begin
  if Hide then
    Exit;
end;

procedure TGUIObject.OnResize;
begin
  SetResize;
end;

procedure TGUIObject.OutHit(pX, pY: Integer);
begin
  RemoveAction([goaDown, goaFocused]);
end;

procedure TGUIObject.ProcessEvents;
begin
  SetFontEvent;
  SetResize;
end;

procedure TGUIObject.Render;

  procedure GapOccur(Item: TVertexClass);
  begin
    if not Item.GapOccur then
      Exit;

    glEnd;
    glBegin(FModeDraw);
  end;

var FID : Integer;
    Item: TVertexClass;
begin
  if (not Assigned(FVertexList)) or FHide then
    Exit;

  //Текстура
  if (Assigned(FTextureLink))   and
     (FTextureLink.Enable)      and
     (not FTextureLink.IsEmpty) then
  begin
    glEnable(GL_TEXTURE_2D);
    FTextureLink.Bind;
  end
  else
    glDisable(GL_TEXTURE_2D);

  //Смешивание цветов
  Blend.Bind;

  if rpSkipScissor in FRenderProps then
    glDisable(GL_SCISSOR_TEST);

  glPushMatrix;
    //Можно сделать glTranslatef(0, 0, 0) и указывать позиию в Rect
    //Но тогда прийдется при перемещении объекта каждый раз пересчитывать все
    //вершины т.к. там позиция не меняется
    glTranslatef(FRect.X, FRect.Y, 0);
    glScalef(FScale, FScale, FScale);

    BeforeObjRender;

    glBegin(FModeDraw);

      for FID := 0 to FVertexList.Count - 1 do
      begin
        Item:= FVertexList.Vertex[FID];

        //Пересчитать координаты, даже у скрытых элементов
        if (goaTextureNeedRecalc in FAction)   or
           (goaTextureAlwaysRecalc in FAction) then
        begin
          Item.TexCoord.SetCalculatedUV(
                Item.TexCoord.U / (FTextureInfo.Width),
              -(Item.TexCoord.V / (FTextureInfo.Height))
            );
        end;

        if Item.Hide then
        begin
          GapOccur(Item);
          Continue;
        end;

        glColor4f   (Item.Color.R, Item.Color.G, Item.Color.B, Blend.Alpha);
        glTexCoord2f(Item.TexCoord.UCalc, Item.TexCoord.VCalc);
        glVertex2f  (Item.Vertex.X, Item.Vertex.Y);

        //Если эта вершина разрывающая начнем рисовать новый элемент
        GapOccur(Item);
      end;

    glEnd;

    //Удалим флаг что нужно пересчитывать текстурные координаты
    RemoveAction([goaTextureNeedRecalc]);
    AfterObjRender;

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  glPopMatrix;

  RenderText;
end;

procedure TGUIObject.RenderText;
begin
  if Hide then
    Exit;

  if FFont._State <> gfsNone then
    SetFontEvent;
end;

procedure TGUIObject.RemoveAction(pAction: TGUIObjectAction);
begin
  FAction:= FAction - pAction;
end;

procedure TGUIObject.SetAction(pAction: TGUIObjectAction);
begin
  FAction:= FAction + pAction;
end;

procedure TGUIObject.SetAreaResize;
begin
  if not Assigned(FArea) then
    Exit;

  FArea.Rect.SetRect(Rect);
end;

procedure TGUIObject.SetColor(pColor: TColor);
var FID: Integer;
begin
  FColor.SetColor(pColor);
  for FID := 0 to FVertexList.Count - 1 do
    FVertexList.Vertex[FID].Color.SetColor(pColor);
end;

procedure TGUIObject.SetEnable(pEnable: Boolean);
begin
  FEnable:= pEnable;
end;

procedure TGUIObject.SetFontEvent;
begin
  if FHide then
    Exit;

  if FFont = nil then
    Exit;

  FFont._State:= gfsNone;
end;

procedure TGUIObject.SetFontLink(pFont: TGUIFont);
begin
  FFont:= pFont;
  FFont._State:= gfsUpdate;
end;

procedure TGUIObject.SetPopupMenu(pPopupMenu: TGUIObject);
begin
  FPopupMenu:= pPopupMenu;
  if FPopupMenu = nil then
    Exit;

  try
    FPopupMenu.Parent:= Self;
  finally
  end;
end;

procedure TGUIObject.SetPos(pX, pY: Integer);
begin
  X:= pX;
  Y:= pY;
end;

procedure TGUIObject.SetTextureCopyFrom(pTextureLink: TTextureLink);
begin
  FTextureLink.CopyFrom(pTextureLink);
  ChangeTextureInfo;
end;

procedure TGUIObject.SetTextureLink(pTextureLink: TTextureLink);
begin
  FTextureLink:= pTextureLink;
  ChangeTextureInfo;
end;

procedure TGUIObject.SetRect(pX, pY, pW, pH: Integer);
begin
  FRect.SetRect(pX, pY, pW, pH);
  SetResize;
end;

procedure TGUIObject.SetRect(pRect: TGUIObjectRect);
begin
  SetRect(pRect.X, pRect.Y, pRect.Width, pRect.Height);
end;

procedure TGUIObject.SetResize;
begin
end;

procedure TGUIObject.SetScale(pScale: TFloat);
begin
  FScale:= pScale;
end;

procedure TGUIObject.SetSize(pWidth, pHeight: Integer);
begin
  FRect.SetSize(pWidth, pHeight);
  SetResize;
end;

procedure TGUIObject.SetWidth(pWidth: Integer);
begin
  if FRect.Width = pWidth then
    Exit;

  FRect.Width:= pWidth;
  SetResize;
end;

procedure TGUIObject.SetHeight(pHeight: Integer);
begin
  if FRect.Height = pHeight then
    Exit;

  FRect.Height:= pHeight;
  SetResize;
end;

procedure TGUIObject.SetHide(pHide: Boolean);
begin
  FHide:= pHide;
  RemoveAction([goaFocused, goaDown]);
end;

procedure TGUIObject.SetGUID(pGUID: String);
begin
  if Trim(FGUID) = '' then
    FGUID:= pGUID;
end;

{ TGUIObjectAlpha }

constructor TGUIObjectAlpha.Create(pValue: TFloat; pSrc, pDst: TUInt);
begin
  SetValue(pValue, pSrc, pDst);
end;

procedure TGUIObjectAlpha.SetValue(pValue: TFloat; pSrc: TUInt = GL_ONE; pDst: TUInt = GL_ONE_MINUS_SRC_ALPHA);
begin
  Value:= pValue;
  Src  := pSrc;
  Dst  := pDst;
end;

{ TGUITextureInfo }

procedure TGUITextureInfo.SetSize(pWidth, pHeight: Integer);
begin
  Width := pWidth;
  Height:= pHeight;
end;

{ TGUIHintObject }

constructor TGUIHintObject.Create;
begin
  FText   := '';
  FColor  := clWhite;
  FEnable := False;
  FBGColor:= $002C1A16;
end;

procedure TGUIHintObject.SetBackgroundColor(pColor: TColor);
begin
  FBGColor:= pColor;
end;

procedure TGUIHintObject.SetColor(pColor: TColor);
begin
  FColor:= pColor;
end;

procedure TGUIHintObject.SetEnable(pEnable: Boolean);
begin
  FEnable:= pEnable;
end;

procedure TGUIHintObject.SetText(pText: String);
begin
  FText:= pText;
end;

{ TGUIMessage }

function TGUIMessage.Make(const AMessage: Integer; ASender: TObject): TGUIMessage;
begin
  Msg   := AMessage;
  Sender:= ASender;
  Result:= Self;
end;

end.
