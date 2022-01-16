unit dlGUIXmlSerial;

interface

uses SysUtils, Classes, TypInfo, XMLIntf, XMLDoc, ComObj, RTTI;

const XMLIdent    = 'GUIXMLSerial';
      XMLObjProc  = 'TGUIProc';
      XMLObjItems = 'Items';
      XMLMethods  = 'Methods';


type
  TXMLSerial = class(TCustomAttribute)

  end;

  //Вызывается каждый раз при создании объекта XMLPropToObject
  TGUIXMLOutObject = reference to procedure(AObject: TObject; AParent: TObject);

  TGUIXMLSerial = class
    strict private
      //Для рекурсии (объект в XML)
      class procedure ObjectToXML(ANode: IXMLNode; AObject: TObject);
      class procedure ObjectPropToXML(AContext: TRttiContext; ANode: IXMLNode; AObject: TObject);
      class procedure ObjectFieldsToXML(AContext: TRttiContext; ANode: IXMLNode; AObject: TObject);

      class procedure SetObjectProps(const AContext: TRttiContext; AItemType: TRttiType; AObject: TObject; ANode: IXMLNode);
      class procedure XMLPropToObject(const AContext: TRttiContext; ANode: IXMLNode; const AParent: TObject; const AProc: TGUIXMLOutObject);
      class function XMLMakeObject(const AContext: TRttiContext; AParent: TObject; ANode: IXMLNode): TObject;
      class function XMLToObject(ANode: IXMLNode; const AProc: TGUIXMLOutObject): Boolean;
    public
      //Сохранить данные String в XML
      class function SaveToXML(const AFileName: String; AData: String): Boolean;
      class function LoadFromXML(const AFileName: String; const AProc: TGUIXMLOutObject): Boolean;

      //Конвертировать класс в XML -> String
      class function GUIObjectToXML(AObject: TObject): String;
  end;

implementation

uses dlGUIForm;

{ TGUIXMLSerial }

class function TGUIXMLSerial.GUIObjectToXML(AObject: TObject): String;
var XMLDoc: IXMLDocument;
begin
  Result:= '';

  if not Assigned(AObject) then
    Exit;

  try
    XMLDoc:= NewXMLDocument();
    XMLDoc.AddChild(XMLIdent);
    ObjectToXML(XMLDoc.DocumentElement, AObject);

    Result:= XMLDoc.DocumentElement.XML;
  finally

  end;
end;

class function TGUIXMLSerial.LoadFromXML(const AFileName: String; const AProc: TGUIXMLOutObject): Boolean;
var XMLDoc: IXMLDocument;
begin
  Result:= False;
  if not FileExists(AFileName) then
    Exit;

  try
    XMLDoc:= LoadXMLDocument(AFileName);
    Result:= XMLToObject(XMLDoc.ChildNodes.FindNode(XMLIdent), AProc);
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
            if SameText(Field.FieldType.Name, TList.ClassName) then
            begin
              BufList:= TList(Field.GetValue(AObject).AsObject);
              if not Assigned(BufList) then
                Continue;

              NodeList:= ANode.AddChild(XMLObjItems);
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

        if not SameText(String(Field.FieldType.Handle^.Name), XMLObjProc) then
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
              Node:= ANode.AddChild(XMLMethods);

            xmlName := Field.Name;
            xmlValue:= MethodItem.Name;

            Node.AddChild(XMLMethods).Attributes[xmlName]:= xmlValue;
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
          tkChar       ,
          tkString     ,
          tkWChar      ,
          tkLString    ,
          tkWString    ,
          tkUString    : ANode.Attributes[xmlName]:= xmlValue;

          tkClass :
          begin
            if SameText(Prop.PropertyType.Name, TList.ClassName) then
            begin
              BufList:= TList(Prop.GetValue(AObject).AsObject);
              if not Assigned(BufList) then
                Continue;

              NodeList:= ANode.AddChild(XMLObjItems);
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
          tkSet        :;
          tkMethod     :;
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

class function TGUIXMLSerial.SaveToXML(const AFileName: String; AData: String): boolean;
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

class procedure TGUIXMLSerial.SetObjectProps(const AContext: TRttiContext; AItemType: TRttiType; AObject: TObject; ANode: IXMLNode);
var Prop    : TRttiProperty;
    ObjValue: TValue;
    Value   : Variant;
    Buf     : String;
begin
  try
    //Присваиваем значения свойствам объекта
    for Prop in AItemType.GetProperties do
    begin
      if not ANode.HasAttribute(Prop.Name) then
        Continue;

      if not Prop.IsWritable then
        Continue;

      Buf  := ANode.Attributes[Prop.Name];
      Value:= ANode.Attributes[Prop.Name];

      case Prop.PropertyType.TypeKind of
          //tkClass      :;
          //tkRecord     :;
          //tkUnknown    :;

          tkInteger    : TValue.Make<Integer>(Value, ObjValue);
          tkInt64      : TValue.Make<Int64>(Value, ObjValue);
          tkEnumeration: begin
                           ObjValue:= TValue.FromOrdinal(
                             Prop.PropertyType.Handle,
                             GetEnumValue(Prop.PropertyType.Handle, Buf)
                           );
                         end;
          tkFloat      : TValue.Make<Double>(Value, ObjValue);
          tkChar, tkUString, tkString, tkWChar, tkLString,
          tkWString    : TValue.Make<String>(Value, ObjValue);
          //tkSet        :;
          //tkMethod     :;
          //tkVariant    :;
          //tkArray      :;
          //tkInterface  :;
          //tkDynArray   :;
          //tkClassRef   :;
          //tkPointer    :;
          //tkProcedure  :;
          //tkMRecord    :;
      end;

      Prop.SetValue(AObject, ObjValue);
    end;
  finally

  end;
end;

class function TGUIXMLSerial.XMLToObject(ANode: IXMLNode; const AProc: TGUIXMLOutObject): Boolean;
var Context : TRttiContext;
begin
  Result:= False;
  if not Assigned(ANode) then
    Exit;

  Context:= TRttiContext.Create;
  try
    //Parent = nil
    XMLPropToObject(Context, ANode, nil, AProc);
    Result:= True;
  finally
    Context.Free;
  end;

end;

class function TGUIXMLSerial.XMLMakeObject(const AContext: TRttiContext; AParent: TObject; ANode: IXMLNode): TObject;
const ATTR_NAME   = 'Name';
      METH_CREATE = 'Create';

var ItemType: TRttiType;
    Instance: TRttiInstanceType;
    ObjValue: TValue;
    Params  : array of TValue;
begin
  Result:= nil;
  if not Assigned(ANode) then
    Exit;

  try
    ItemType:= AContext.FindType(ANode.NodeName);
    //Не нашли подходящий тип (класс)
    if not Assigned(ItemType) then
      Exit;

    Instance:= ItemType.AsInstance;
    if not Assigned(Instance) then
      Exit;

    Params:= nil;
    //Создаем объект конструктором Create
    if ANode.HasAttribute(ATTR_NAME) then
    begin
      TValue.Make<String>(ANode.Attributes[ATTR_NAME], ObjValue);
      Result:= Instance.GetMethod(METH_CREATE).Invoke(Instance.MetaclassType, [ObjValue, nil]).AsObject;
    end
    else
    begin
      SetLength(Params, Length(Instance.GetMethod(METH_CREATE).GetParameters));
      Result:= Instance.GetMethod(METH_CREATE).Invoke(Instance.MetaclassType, Params).AsObject;
    end;

    SetObjectProps(AContext, ItemType, Result, ANode);

  finally

  end;
end;

class procedure TGUIXMLSerial.XMLPropToObject(const AContext: TRttiContext; ANode: IXMLNode; const AParent: TObject; const AProc: TGUIXMLOutObject);
var i: integer;
    Obj: TObject;
    err: string;
    errNode: IXMLNode;
begin
  try
    Obj:= nil;

    //Пропускаем ноду с названием Items
    if not SameText(XMLObjItems, ANode.NodeName) then
    begin
      Obj:= XMLMakeObject(AContext, AParent, ANode);

      if Assigned(AProc) then
        AProc(Obj, AParent);
    end;

    if SameText(XMLMethods, ANode.NodeName) then
    begin
      //Чтение методов
      for i := 0 to ANode.ChildNodes.Count - 1 do
        //Пока что не понятно как читать
      Exit;
    end
    else
      for i := 0 to ANode.ChildNodes.Count - 1 do
        XMLPropToObject(AContext, ANode.ChildNodes.Nodes[i], Obj, AProc);
  except
    on e: exception do
    begin
      err:= e.Message;
      errNode:= ANode;
    end;
  end;

end;

end.
