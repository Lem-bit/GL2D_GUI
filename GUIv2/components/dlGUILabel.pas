unit dlGUILabel;

interface

uses dlGUITypes, dlGUIObject;

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

type
  TGUILabel = class(TGUIObject)
    private
      FText    : String;  //Текст
      FWordWarp: Boolean; //Переносить текст на след строку или нет
    private
      procedure SetText(pText: String);
    public
      constructor Create(pText: String; pName: String = '');
      procedure RenderText; override;
    published
      property ObjectType;
      property Name;
      property X;
      property Y;
      property Font;
      property Hide;
      property TextureName;
      //классы
      property Parent;
      property PopupMenuName;
      property Hint;
      property Blend;

      property Text    : String  read FText     write SetText;
      property WordWarp: Boolean read FWordWarp write FWordWarp;
  end;

implementation

{ TGUILabel }

constructor TGUILabel.Create(pText: String; pName: String = '');
begin
  inherited Create(pName, gtcLabel);
  FWordWarp:= False;
  FText    := pText;
end;

procedure TGUILabel.RenderText;
begin
  inherited;
  FFont.RenderText(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FText, Rect.Width, FWordWarp);
end;

procedure TGUILabel.SetText(pText: String);
begin
  if FText = pText then
    Exit;

  FText:= pText;
  SetFontEvent;
end;

end.
