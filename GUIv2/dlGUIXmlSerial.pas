unit dlGUIXmlSerial;

interface

uses SysUtils, Classes, TypInfo, XMLIntf, XMLDoc, ComObj, RTTI;

type
  TXMLSerial = class(TCustomAttribute)

  end;

  TGUIXMLSerial = class
    strict private
      //Для рекурсии (объект в XML)
      class procedure ObjectToXML(ANode: IXMLNode; AObject: TObject);
      class procedure ObjectPropToXML(AContext: TRttiContext; ANode: IXMLNode; AObject: TObject);
      class procedure ObjectFieldsToXML(AContext: TRttiContext; ANode: IXMLNode; AObject: TObject);

      //class procedure XMLToObject(ANode: IXMLNode; var AObject: TObject);
    public
      //Сохранить данные String в XML
      class function SaveToXML(AData: String; AFileName: String): Boolean;
      class function LoadFromXML(AFileName: String): String;
      //Конвертировать класс в XML -> String
      class function GUIObjectToXML(AObject: TObject): String;
      //Конвертировать строку в XML -> Object
      //class function XMLToGUIObject(AData: String; var AObject: TObject): Boolean;
  end;

implementation

{ TGUIXMLSerial }

class function TGUIXMLSerial.GUIObjectToXML(AObject: TObject): String;
var XMLDoc: IXMLDocument;
begin
  Result:= '';

  if not Assigned(AObject) then
    Exit;

  try
    XMLDoc:= NewXMLDocument();
    XMLDoc.AddChild('GUIXMLSerial');
    ObjectToXML(XMLDoc.DocumentElement, AObject);

    Result:= XMLDoc.DocumentElement.XML;
  finally

  end;
end;

class function TGUIXMLSerial.LoadFromXML(AFileName: String): String;
var XMLDoc: IXMLDocument;
begin
  Result:= '';

  if not FileExists(AFileName) then
    Exit;

  try
    XMLDoc:= LoadXMLDocument(AFileName);
    Result:= XMLDoc.DocumentElement.XML;
  except

  end;
end;

class procedure TGUIXMLSerial.ObjectFieldsToXML(AContext: TRttiContext; ANode: IXMLNode; AObject: TObject);
var ItemType  : TRttiType;
    Node      : IXMLNode;
    Field     : TRttiField;
    FieldRec  : TRttiField;
    Value     : TValue;
    xmlName   : String;
    xmlValue  : String;
    MethodItem: TRttiMethod;
    Method    : TMethod;
    ProcType  : TRttiType;
    Attr      : TCustomAttribute;
    BufList   : TList;
    NodeList  : IXMLNode;
    i         : integer;
    Instance  : Pointer;
    Rec       : TRttiRecordType;
begin
  if not Assigned(ANode) then
    Exit;

  if not Assigned(AObject) then
    Exit;

  try
    Node:= nil;
    ItemType:= AContext.GetType(AObject.ClassType);

    for Field in ItemType.GetFields do
      for Attr in Field.GetAttributes do
      begin
        if not (Attr is TXMLSerial) then
          Continue;

        if not Assigned(Field.FieldType) then
          Continue;

        xmlName := Field.Name;
        xmlValue:= Field.GetValue(AObject).ToString;

        case Field.FieldType.TypeKind of
          tkInteger    ,
          tkEnumeration,
          tkFloat      ,
          tkUString    : ANode.Attributes[xmlName]:= xmlValue;

          tkClass :
          begin
            if Field.FieldType.Name = TList.ClassName then
            begin
              BufList:= TList(Field.GetValue(AObject).AsObject);
              if not Assigned(BufList) then
                Continue;

              NodeList:= ANode.AddChild('Items');
              for i := 0 to BufList.Count - 1 do
                ObjectToXML(NodeList, TObject(BufList[i]));

              Continue;
            end;

            if Field.GetValue(AObject).IsObject then
              ObjectToXML(ANode, Field.GetValue(AObject).AsObject);
          end;

          tkRecord: begin
            Instance:= Field.GetValue(AObject).GetReferenceToRawData;
            Rec:= AContext.GetType(Field.FieldType.Handle).AsRecord;

            for FieldRec in Rec.GetFields do
            begin
              xmlName := FieldRec.Name;
              xmlValue:= FieldRec.GetValue(Instance).ToString;
              ANode.Attributes[xmlName]:= xmlValue;
            end;
          end;
        end;

        //Methods
        if not Assigned(Field.FieldType.Handle) then
          Continue;

        if Field.FieldType.Handle^.Kind <> tkMethod then
          Continue;

        if not SameText(String(Field.FieldType.Handle^.Name), 'TGUIProc') then
          Continue;

        Value:= Field.GetValue(AObject);

        if Value.Kind <> tkMethod then
          Continue;

        Method:= TMethod(Value.GetReferenceToRawData^);
        if Method.Code = nil then
          Continue;

        ProcType:= AContext.GetType(TObject(Method.Data).ClassType);

        for MethodItem in ProcType.GetMethods do
          if MethodItem.CodeAddress = Method.Code then
          begin
            if not Assigned(Node) then
              Node:= ANode.AddChild('Methods');

            xmlName := Field.Name;
            xmlValue:= MethodItem.Name;

            Node.AddChild('Method').Attributes[xmlName]:= xmlValue;
            Break;
          end;
      end;
  finally

  end;

end;

class procedure TGUIXMLSerial.ObjectPropToXML(AContext: TRttiContext; ANode: IXMLNode; AObject: TObject);
var ItemType: TRttiType;
    Prop    : TRttiProperty;
    Attr    : TCustomAttribute;
    Rec     : TRttiRecordType;
    Field   : TRttiField;
    Instance: Pointer;
    NodeList: IXMLNode;
    xmlName : String;
    xmlValue: String;
    BufList : TList;
    i       : integer;
begin
  if not Assigned(ANode) then
    Exit;

  if not Assigned(AObject) then
    Exit;

  try
    ItemType:= AContext.GetType(AObject.ClassType);

    for Prop in ItemType.GetProperties do
      for Attr in Prop.GetAttributes do
      begin
        if not (Attr is TXMLSerial) then
          Continue;

        xmlName := Prop.Name;
        xmlValue:= Prop.GetValue(AObject).ToString;

        case Prop.PropertyType.TypeKind of
          tkInteger    ,
          tkEnumeration,
          tkFloat      ,
          tkUString    : ANode.Attributes[xmlName]:= xmlValue;

          tkClass :
          begin
            if Prop.PropertyType.Name = TList.ClassName then
            begin
              BufList:= TList(Prop.GetValue(AObject).AsObject);
              if not Assigned(BufList) then
                Continue;

              NodeList:= ANode.AddChild('Items');
              for i := 0 to BufList.Count - 1 do
                ObjectToXML(NodeList, TObject(BufList[i]));

              Continue;
            end;

            if Prop.GetValue(AObject).IsObject then
              ObjectToXML(ANode, Prop.GetValue(AObject).AsObject);
          end;

          tkRecord: begin
            Instance:= Prop.GetValue(AObject).GetReferenceToRawData;
            Rec:= AContext.GetType(Prop.PropertyType.Handle).AsRecord;

            for Field in Rec.GetFields do
            begin
              xmlName := Field.Name;
              xmlValue:= Field.GetValue(Instance).ToString;
              ANode.Attributes[xmlName]:= xmlValue;
            end;
          end;

          tkUnknown    :;
          tkChar       :;
          tkString     :;
          tkSet        :;
          tkMethod     :;
          tkWChar      :;
          tkLString    :;
          tkWString    :;
          tkVariant    :;
          tkArray      :;
          tkInterface  :;
          tkInt64      :;
          tkDynArray   :;
          tkClassRef   :;
          tkPointer    :;
          tkProcedure  :;
          tkMRecord    :;
        end;

      end;
  finally

  end;
end;

class procedure TGUIXMLSerial.ObjectToXML(ANode: IXMLNode; AObject: TObject);
var Context: TRttiContext;
    Node: IXMLNode;
begin
  if not Assigned(ANode) then
    Exit;

  if not Assigned(AObject) then
    Exit;

  Context:= TRttiContext.Create;

  try
    Node:= ANode.AddChild(AObject.QualifiedClassName);
    //Properties
    ObjectPropToXML(Context, Node, AObject);
    //Fields + Methods
    ObjectFieldsToXML(Context, Node, AObject);
  finally
    Context.Free;
  end;
end;

class function TGUIXMLSerial.SaveToXML(AData, AFileName: String): boolean;
var XMLDoc: IXMLDocument;
begin
  Result:= False;
  if Trim(AData) = '' then
    Exit;

  try
    XMLDoc:= LoadXMLData(AData);
    XMLDoc.SaveToFile(AFileName);

    Result:= True;
  except
    //без обработки
  end;
end;
{
class function TGUIXMLSerial.XMLToGUIObject(AData: String; var AObject: TObject): Boolean;
var XMLDoc: IXMLDocument;
begin

  try
    XMLDoc:= LoadXMLData(AData);
    XMLToObject(XMLDoc.ChildNodes.FindNode('GUIXMLSerial'), AObject);

    Result:= True;
  finally

  end;
end;

class procedure TGUIXMLSerial.XMLToObject(ANode: IXMLNode; var AObject: TObject);
var Context : TRttiContext;
    ItemType: TRttiType;
    Instance: TRttiInstanceType;
begin
  if not Assigned(ANode) then
    Exit;

  Context:= TRttiContext.Create;

  try
    ItemType:= Context.FindType('dlGUILabel.TGUILabel');
    if ItemType <> nil then
    begin
      Instance:= ItemType.AsInstance;
      AObject:= Instance.GetMethod('Create').Invoke(Instance.MetaclassType, [nil, nil]).AsObject;
    end;

  finally
    Context.Free;
  end;
end;  }

end.
