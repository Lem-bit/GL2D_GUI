unit dlGUIProcList;

interface

 //Список функций и процедур которые привязываются к GUIObject

uses SysUtils, classes, dlGUITypes;

{
  ====================================================
  = Delphi OpenGL GUIv2                              =
  =                                                  =
  = Author: Ansperi L.L., 2021                       =
  = Email : gui_proj@mail.ru                         =
  = Site  : lemgl.ru                                 =
  =                                                  =
  ====================================================
}

 type
   TGUIProcClass = class
     public
       Name: String;   //Название процедуры
       Proc: TGUIProc; //Указатель на процедуру
     public
       constructor Create(pName: String; pProc: TGUIProc);
   end;

   TGUIProcList = class
     private
       FProc: TList;

       function SearchIndex(pProcName: String): Integer;
       function Void(num: integer): Boolean;
     public
       constructor Create;

       //Запросить процедуру номер Х
       function GetProc(pIndex: integer): TGUIProc;

       //Добавить процедуру
       function AddProc(pProcName: String; pProc: TGUIProc): Integer;

       //Найти процедуру
       function SearchProc(pProcName: String): TGUIProc;

       //Удалить процедуру
       function DeleteProc(pProcName: String): Boolean;

       //Список
       property Proc: TList read FProc;

       //Уничтожить список
       destructor Destroy; override;
   end;

implementation

{ TProcList }

constructor TGUIProcList.Create;
begin
  FProc:= TList.Create;
end;

function TGUIProcList.AddProc(pProcName: String; pProc: TGUIProc): Integer;
begin
  Result:= -1;
   if Trim(pProcName) = '' then
     Exit;

   if SearchIndex(pProcName) <> -1 then
     Exit;

  Result:= FProc.Add(TGUIProcClass.Create(pProcName, pProc));
end;

function TGUIProcList.SearchIndex(pProcName: String): Integer;
var FID: Integer;
begin
  Result:= -1;

  for FID := 0 to FProc.Count - 1 do
    if SameText(pProcName, TGUIProcClass(FProc.Items[FID]).Name) then
    begin
      Result:= FID;
      Break;
    end;

end;

function TGUIProcList.SearchProc(pProcName: String): TGUIProc;
var FID: Integer;
begin
  Result:= nil;

  FID:= SearchIndex(pProcName);

  if not Void(FID) then
    Exit;

  Result:= TGUIProcClass(FProc.Items[FID]).Proc;
end;


function TGUIProcList.Void(num: integer): Boolean;
begin
  Result:= False;

  if (num < 0) or (num > FProc.Count) then
    Exit;

  Result:= True;
end;

function TGUIProcList.DeleteProc(pProcName: String): Boolean;
var FID: integer;
begin
  Result:= False;

  FID:= SearchIndex(pProcName);
  if FID = -1 then
    Exit;

  TGUIProcClass(FProc.Items[FID]).Free;

  FProc.Delete(FID);
  FProc.Pack;
end;

destructor TGUIProcList.Destroy;
var FID : Integer;
begin
  if Assigned(FProc) then
    for FID := 0 to FProc.Count - 1 do
      TGUIProcClass(FProc.Items[FID]).Destroy;

  FreeAndNil(FProc);
  inherited;
end;

function TGUIProcList.GetProc(pIndex: integer): TGUIProc;
begin
  Result:= nil;
  if not Void(pIndex) then
    Exit;

  Result:= TGUIProcClass(FProc.Items[pIndex]).Proc;
end;

{ TProcClass }

constructor TGUIProcClass.Create(pName: String; pProc: TGUIProc);
begin
  Name:= pName;
  Proc:= pProc;
end;

end.

