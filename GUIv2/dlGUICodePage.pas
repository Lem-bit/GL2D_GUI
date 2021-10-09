unit dlGUICodePage;

interface

uses SysUtils;

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

(*
   Для перекодирования кодовой страницы 1251, 1252 и т.д.
   чтобы корректно отображались русские символы
*)

type
  AnsiStr = type AnsiString(1251);

  TGUILocale = class
    public
      //Преобразовать из символа в число
      function GetIndexChar(pChar: char): Byte;
      //Преобразовать из числа в символ
      function ToAnsiChar(pCharID: Word): String;
      //Преобразовать из Ansi в нужный символ
      function ToLocalChar(pCharID: Word): String;
  end;

  var GUILocale: TGUILocale;

implementation

{ TGUILang }

function TGUILocale.GetIndexChar(pChar: char): Byte;
begin
  Result:= ord(AnsiStr(pChar)[1]);;
end;

function TGUILocale.ToAnsiChar(pCharID: Word): String;
begin
  Result:= String(AnsiStr(AnsiChar(char(pCharID))));
end;

function TGUILocale.ToLocalChar(pCharID: Word): String;
begin
  Result:= String(AnsiStr(AnsiChar(pCharID)));
end;

initialization
  GUILocale:= TGUILocale.Create;

finalization
  if Assigned(GUILocale) then
    FreeAndNil(GUILocale);


end.
