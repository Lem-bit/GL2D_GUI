unit dlGUIPropToXML;

interface

uses SysUtils, Classes, ActiveX, XMLIntf, XMLDoc, ComObj, dlGUIFormList, dlGUITypesRTTI, dlGUIObject;

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

const attr_Component = 'Component';
      attr_Field     = 'Field';
      attr_Class     = 'Class';
      attr_PopupMenu = 'PopupMenu';
      attr_ProcList  = 'ProcList';
      attr_Proc      = 'Proc';
      attr_ItemList  = 'ItemList';
      attr_Item      = 'Item';

      attr_FormList  = 'FormList';
      attr_Form      = 'Form';

type
  TGUIPropToXML = class
      FFileName: String;
    private
      procedure TextObjectToXML(pData: String; pNode: IXMLNode);
    public
      constructor Create(pFileName: String);
      //Экспорт в XML
      procedure ExportFormList(pFormList: TGUIFormList);
      //Импорт из XML
      procedure ImportFromXML(pFormList: TGUIFormList);
  end;

implementation

{ TGUIPropToXML }

constructor TGUIPropToXML.Create(pFileName: String);
begin
  FFileName:= pFileName;
end;

procedure TGUIPropToXML.ImportFromXML(pFormList: TGUIFormList);
var xmlDoc: IXMLDocument;
    INode : IXMLNode;
begin
  try
    CoInitialize(nil);
    xmlDoc:= LoadXMLDocument(FFileName);
    xmlDoc.Active:= True;

    INode:= xmlDoc.Node;
    INode.HasAttribute('value')
  finally

  end;

end;


procedure TGUIPropToXML.TextObjectToXML(pData: String; pNode: IXMLNode);
var i: integer;
    Lines: TStringList;
    Buf: String;

    Raw: TObjRTTIRawData;

    CurrNode: IXMLNode;
begin

  Lines:= TStringList.Create;

  try
    Lines.Text:= pData;
    CurrNode  := pNode;

    for i := 0 to Lines.Count - 1 do
    begin
      //Строка
      Buf:= Lines[i];
      if Trim(Buf) = '' then Continue;

      //По типу определяем что делаем с нодой
      Raw:= objRTTI.LineToRawData(Buf);
      if Raw.RawType = rawNull then Continue;

      //Обработка нод
      case Raw.RawType of
        rawClass      : begin
                          CurrNode:= CurrNode.AddChild(attr_Field);
                          CurrNode.Attributes[Raw.Name]:= Raw.Value;
                        end;
        rawProc       : CurrNode:= CurrNode.AddChild(attr_Proc);
        rawItemList   : CurrNode:= CurrNode.AddChild(attr_ItemList);
        rawItem       : CurrNode:= CurrNode.AddChild(attr_Item);

        rawData       : CurrNode.Attributes[Raw.Name]:= Raw.Value;

        rawEndItem    : CurrNode:= CurrNode.ParentNode;
        rawEndItemList: CurrNode:= CurrNode.ParentNode;
        rawEndProc    : CurrNode:= CurrNode.ParentNode;
        rawEndClass   : CurrNode:= CurrNode.ParentNode;
      end;

    end;

  finally
    Lines.Free;
  end;

end;

procedure TGUIPropToXML.ExportFormList(pFormList: TGUIFormList);

var //Popup обрабатываем отдельно так как может быть 1 ко многим
    PopupList: TList;

procedure AddPopupToList(pObject: TObject);
begin
  if pObject = nil then Exit;
  if PopupList.IndexOf(pObject) > -1 then Exit;

  PopupList.Add(pObject);
end;

var i, j     : integer;
    xmlDoc   : IXMLDocument;
    MainNode : IXMLNode;
    FormNode : IXMLNode;
    CompNode : IXMLNode;
begin
  if pFormList = nil then Exit;

  PopupList:= TList.Create;

  try
    CoInitialize(nil);
    xmlDoc:= TXMLDocument.Create(nil);
    xmlDoc.Active:= True;
    xmlDoc.DocumentElement:= xmlDoc.CreateNode(attr_FormList, ntElement, '');

    MainNode:= xmlDoc.ChildNodes.FindNode(attr_FormList);

    for i := 0 to pFormList.Count - 1 do
    begin
      //Добавим форму
      FormNode:= MainNode.AddChild(attr_Form);
      //Добавим все published свойства
      TextObjectToXML(pFormList.Form[i].RTTIGetPublishedList(), FormNode);
      TextObjectToXML(pFormList.Form[i].RTTIGetProcList, FormNode);
      AddPopupToList(pFormList.Form[i].PopupMenu);

      //Добавим все компоненты на форме
      for j := 0 to pFormList.Form[i].ComponentList.Count - 1 do
      begin
        //Добавим компонент
        CompNode:= FormNode.AddChild(attr_Component);
        //Добавим все published свойства на компоненте
        if pFormList.Form[i].GetComponent(j) = nil then Continue;

        with pFormList.Form[i] do
        begin
          TextObjectToXML(GetComponent(j).RTTIGetPublishedList, CompNode);
          TextObjectToXML(GetComponent(j).RTTIGetProcList, CompNode);
          AddPopupToList(GetComponent(j).PopupMenu);
        end;
      end;

    end;

    FormNode:= MainNode.AddChild(attr_PopupMenu);
    for i := 0 to PopupList.Count - 1 do
    begin
      TextObjectToXML(TGUIObject(PopupList.Items[i]).RTTIGetPublishedList(), FormNode);
    end;


    xmlDoc.SaveToFile(FFileName);
  finally
    PopupList.Free;
  end;
end;


end.
