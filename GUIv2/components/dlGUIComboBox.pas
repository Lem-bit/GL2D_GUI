unit dlGUIComboBox;

interface

uses Classes, SysUtils, dlGUITypes, dlGUIObject, dlGUIEditBox, dlGUIButton, dlGUIListBox, dlGUIPaletteHelper,
  dlGUIXMLSerial;

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
  TGUIComboBox = class(TGUIObject)
    strict private
      [TXMLSerial] FComponent : TList;
      FItemIndex : Integer; //Выбранный элемент меню
      FButtonSize: Integer;
      FListHeight: Integer; //Высота списка
    strict private
      type
        TElement = (eEdit, eButton, eList);

      function FEdit: TGUIEditBox;
      function FButton: TGUIButton;
      function FList: TGUIListBox;

      function CheckAssignedComponentList: Boolean;

      procedure OnButtonClick(Sender: TObject; ParamObj: Pointer = nil);
      procedure OnElementSelect(Sender: TObject; ParamObj: Pointer = nil);
      procedure ChangeButtonImage;

      function GetItem(value: integer): TGUIListBoxItem;
      function GetText: String;
      procedure SetText(value: string);
    protected
       //Событие при изменении шрифта
       procedure SetAreaResize; override;
       procedure SetFontEvent; override;
       procedure SetResize; override;
    public
       //Установить ссылку на текстуру
       procedure SetTextureLink(pTextureLink: TTextureLink); override;
    public
      constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);

      destructor Destroy; override;

      procedure OnKeyDown(var Key: Word; Shift: TShiftState); override;
      procedure OnKeyUp(var Key: Word; Shift: TShiftState); override;
      procedure OnKeyPress(Key: Char); override;

      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer); override;
      procedure OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer); override;
      function OnHit(pX, pY: Integer): Boolean; override;
      procedure OutHit(pX, pY: Integer); override;
      procedure OnMouseOver(pX, pY: Integer); override;

      procedure Render; override;
      procedure RenderText; override;
    public
      procedure Add(const pText: String);
      function IndexOf(const pText: String): Integer;
      procedure Delete(const pIndex: Integer);
      function Count: integer;
      procedure Clear;

      procedure LoadFromFile(const AFileName: String);
      procedure SaveToFile(const AFileName: String);
    public
      property Item[index: integer]: TGUIListBoxItem read GetItem;
      [TXMLSerial] property Text     : String  read GetText write SetText;
      [TXMLSerial] property ItemIndex: Integer read FItemIndex;
  end;

implementation

{ TGUIComboBox }

procedure TGUIComboBox.Add(const pText: String);
begin
  if Assigned(FComponent.Items[Ord(TElement.eList)]) then
    FList.Add(pText);
end;

procedure TGUIComboBox.ChangeButtonImage;
var ImgIndex: Integer;
begin
  if FList.Hide then
    ImgIndex:= pal_ArrowDn
  else
    ImgIndex:= pal_ArrowUp;

  FButton.VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(ImgIndex));
  FButton.VertexList.SetVertexTextureMap(8, GUIPalette.GetCellRect(ImgIndex));
  FButton.SetAction([goaTextureNeedRecalc]);
end;

procedure TGUIComboBox.Clear;
begin
  FItemIndex:= -1;
  if Assigned(FComponent.Items[Ord(TElement.eList)]) then
    FList.Clear;
end;

function TGUIComboBox.Count: integer;
begin
  if Assigned(FComponent.Items[Ord(TElement.eList)]) then
    Result:= FList.Count
  else
    Result:= 0;
end;

constructor TGUIComboBox.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcComboBox);

  FComponent:= TList.Create;
  SetRect(0, 0, 150, 19);

  FButtonSize:= Rect.Height;
  Area.Show  := True;

  FComponent.Add(TGUIEditBox.Create('cbEdit' , pTextureLink));
  FComponent.Add(TGUIButton.Create('cbButton', pTextureLink));
  FComponent.Add(TGUIListBox.Create('cbList' , pTextureLink));

  SetTextureLink(pTextureLink);

  FEdit.Area.Show  := False;
  FButton.Area.Show:= False;
  FButton.Flat     := True;

  FList.Hide:= True;
  FList.LineSpacing:= 1;
  FList.OnElementSelect:= OnElementSelect;

  FButton.OnClick:= OnButtonClick;
  ChangeButtonImage;

  FShowOnTop := True;
  FListHeight:= 200;

  SetResize;
end;

procedure TGUIComboBox.Delete(const pIndex: Integer);
begin
  if Assigned(FComponent.Items[Ord(TElement.eList)]) then
    FList.Delete(pIndex);
end;

destructor TGUIComboBox.Destroy;
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent.Items[i]).Free;

  FreeAndNil(FComponent);

  inherited;
end;

function TGUIComboBox.CheckAssignedComponentList: Boolean;
begin
  Result:= False;

  if FComponent.Count <> 3 then
    Exit;

  Result:= True;
end;

function TGUIComboBox.FButton: TGUIButton;
begin
  Result:= nil;

  if not CheckAssignedComponentList then
    Exit;

  Result:= TGUIButton(FComponent.Items[Ord(TElement.eButton)]);
end;

function TGUIComboBox.FEdit: TGUIEditBox;
begin
  Result:= nil;

  if not CheckAssignedComponentList then
    Exit;

  Result:= TGUIEditBox(FComponent.Items[Ord(TElement.eEdit)]);
end;

function TGUIComboBox.FList: TGUIListBox;
begin
  Result:= nil;

  if not CheckAssignedComponentList then
    Exit;

  Result:= TGUIListBox(FComponent.Items[Ord(TElement.eList)]);
end;

function TGUIComboBox.GetItem(value: integer): TGUIListBoxItem;
begin
  if Assigned(FComponent.Items[Ord(TElement.eList)]) then
    Result:= FList.Items[value]
  else
    Result:= nil;
end;

function TGUIComboBox.GetText: String;
begin
  Result:= FEdit.Text;
end;

function TGUIComboBox.IndexOf(const pText: String): Integer;
begin
  Result:= -1;
  if not Assigned(FComponent.Items[Ord(TElement.eList)]) then
    Exit;

  Result:= FList.IndexOf(pText);

end;

procedure TGUIComboBox.LoadFromFile(const AFileName: String);
begin
  FItemIndex:= -1;
  if Assigned(FComponent.Items[Ord(TElement.eList)]) then
    FList.LoadFromFile(AFileName);
end;

procedure TGUIComboBox.OnButtonClick(Sender: TObject; ParamObj: Pointer);
begin
  FItemIndex:= -1;

  if not FEnable then
    Exit;

  FList.Height:= FListHeight;
  FList.Hide:= not FList.Hide;
  FList.VisibleVTracker(not FList.Hide);
  ChangeButtonImage;

  //При выборе значения записываем его в Edit.Text
  if Assigned(ParamObj) then
  begin
    FItemIndex:= Integer(ParamObj^);
    Text      := FList.Items[FItemIndex].Text;
  end;

  if FList.Hide then
    Exit;

  //Фокус на лист
  FList.SetAction([goaFocused]);
end;

procedure TGUIComboBox.OutHit(pX, pY: Integer);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OutHit(pX, pY);

  //Скрыть лист
  FList.Hide:= True;
  ChangeButtonImage;
end;

procedure TGUIComboBox.OnElementSelect(Sender: TObject; ParamObj: Pointer);
begin
  OnButtonClick(Sender, ParamObj);
end;

procedure TGUIComboBox.OnKeyDown(var Key: Word; Shift: TShiftState);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OnKeyDown(Key, Shift);
end;

procedure TGUIComboBox.OnKeyPress(Key: Char);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OnKeyPress(Key);
end;

procedure TGUIComboBox.OnKeyUp(var Key: Word; Shift: TShiftState);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OnKeyUp(Key, Shift);
end;

function TGUIComboBox.OnHit(pX, pY: Integer): Boolean;
begin
  Result:= FEdit.OnHit(pX, pY);
  if Result then
    Exit;

  Result:= FButton.OnHit(pX, pY);
  if Result then
    Exit;

  Result:= FList.OnHit(pX, pY);
end;

procedure TGUIComboBox.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  if not FEnable then
    Exit;

  for i := 0 to FComponent.Count - 1 do
    if TGUIObject(FComponent[i]).OnHit(pX, pY) then
    begin
      TGUIObject(FComponent[i]).SetAction([goaFocused]);
      TGUIObject(FComponent[i]).OnMouseDown(pX, pY, Button);
    end
    else
      TGUIObject(FComponent[i]).RemoveAction([goaFocused]);
end;

procedure TGUIComboBox.OnMouseMove(pX, pY: Integer);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OnMouseMove(pX, pY);

  inherited;
end;

procedure TGUIComboBox.OnMouseOver(pX, pY: Integer);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OnMouseOver(pX, pY);

  inherited;
end;

procedure TGUIComboBox.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
 { if goaDown in FEdit.GetAction then
    OnButtonClick(nil, nil);}

  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OnMouseUp(pX, pY, Button);
end;

procedure TGUIComboBox.OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OnMouseWheelDown(Shift, MPosX, MPosY);
end;

procedure TGUIComboBox.OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).OnMouseWheelUp(Shift, MPosX, MPosY);
end;

procedure TGUIComboBox.Render;
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).Render;

  inherited;
end;

procedure TGUIComboBox.RenderText;
var i: integer;
begin
  inherited;

  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).RenderText;
end;

procedure TGUIComboBox.SaveToFile(const AFileName: String);
begin
  FItemIndex:= -1;
  if Assigned(FComponent.Items[Ord(TElement.eList)]) then
    FList.SaveToFile(AFileName);
end;

procedure TGUIComboBox.SetAreaResize;
begin
  Area.Rect.SetRect(0, 0, Width, FButtonSize);
end;

procedure TGUIComboBox.SetFontEvent;
var i: integer;
begin
  inherited;

  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).Font.CopyFrom(Self.Font);
end;

procedure TGUIComboBox.SetResize;
begin
  inherited;

  if FEdit <> nil then
    FEdit.SetRect(X, Y, Width - FButtonSize, FButtonSize);

  if FButton <> nil then
    FButton.SetRect(X + Width - FButtonSize, Y, FButtonSize, FButtonSize);

  if FList <> nil then
    FList.SetRect(X, Y + FButtonSize + 1, Width, Height - FButtonSize);
end;

procedure TGUIComboBox.SetText(value: string);
begin
  FEdit.Text:= value;
end;

procedure TGUIComboBox.SetTextureLink(pTextureLink: TTextureLink);
var i: integer;
begin
  for i := 0 to FComponent.Count - 1 do
    TGUIObject(FComponent[i]).SetTextureLink(pTextureLink);
end;

end.
