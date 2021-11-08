unit dlGUIRadioButton;

interface

uses dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlGUIXmlSerial;

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
  TGUIRadioButton = class(TGUIObject)
    strict private
      FChecked: Boolean;
      FText   : String;
      FGroup  : Byte; //Группа в которой состоит компонент (для переключения)
    strict private
      procedure ChangeStatus(pChecked: Boolean);
    protected
      procedure SetAreaResize; override;
      procedure SetFontEvent; override; //Событие при создании шрифта
    public
      constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
      procedure RenderText; override;

      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure SendGUIMessage(pMessage: TGUIMessage); override;
    public
      [TXMLSerial] property Checked: Boolean read FChecked write ChangeStatus;
      [TXMLSerial] property Group  : Byte    read FGroup   write FGroup;
      [TXMLSerial] property Text   : String  read FText    write FText;
  end;

implementation

const GROUP_UNCHECK    = 0;
      GROUP_CHECK      = 1;
      RADIOBUTTON_SIZE = 16;

{ TGUIRadioButton }

procedure TGUIRadioButton.ChangeStatus(pChecked: Boolean);
begin
  FChecked:= pChecked;
  VertexList.SetGroupHide(GROUP_UNCHECK, pChecked);
  VertexList.SetGroupHide(GROUP_CHECK  , not pChecked);
end;

constructor TGUIRadioButton.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcRadioButton);

  SetRect(0, 0, 100, RADIOBUTTON_SIZE);

  FText    := '';
  FGroup   := 0;
  Area.Show:= True;

  FTextOffset.SetRect(RADIOBUTTON_SIZE + 4, 0, 0, 0);
  SetTextureLink(pTextureLink);

  VertexList.MakeSquare(Rect.X, Rect.Y, RADIOBUTTON_SIZE, RADIOBUTTON_SIZE, Color, GUIPalette.GetCellRect(pal_RadioButton_uc), GROUP_UNCHECK);
  VertexList.MakeSquare(Rect.X, Rect.Y, RADIOBUTTON_SIZE, RADIOBUTTON_SIZE, Color, GUIPalette.GetCellRect(pal_RadioButton_ch), GROUP_CHECK, True);
end;

procedure TGUIRadioButton.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var GUIMessage: TGUIMessage;
begin
  inherited;

  if Button <> gmbLeft then
    Exit;

  if Assigned(Parent) then
   if not Checked then
   begin
     GUIMessage.Msg := MSG_CHNG_RADIOBUTTON;
     GUIMessage.Self:= Self;

     Parent.SendGUIMessage(GUIMessage);
   end;

  Checked:= True;
end;

procedure TGUIRadioButton.RenderText;
begin
  inherited;

  Font.Text(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FText, Rect.Width);
end;

procedure TGUIRadioButton.SendGUIMessage(pMessage: TGUIMessage);
begin
  case pMessage.Msg of
    MSG_CHNG_RADIOBUTTON:
      if pMessage.Self <> Self then
        if TGUIRadioButton(pMessage.Self).Group = Self.Group then
          ChangeStatus(false);
  end;

end;

procedure TGUIRadioButton.SetAreaResize;
begin
  Area.Rect.SetSize(RADIOBUTTON_SIZE, RADIOBUTTON_SIZE);
end;

procedure TGUIRadioButton.SetFontEvent;
begin
  inherited;

  FTextOffset.Y:= -Trunc((Font.Height - RADIOBUTTON_SIZE) / 2) - 1;
end;

end.
