# OpenGL graphic user interface
  
#### Author: Ansperi L.L., 2021
#### Email: gui_proj@mail.ru
#### Site: http://lemgl.ru
#### Telegram: https://t.me/delphi_lemgl

---
Собрано на Delphi 10.4 community

---

`p.s:` 
`Отрисовка использует старый формат вывода изображения через
glBegin/glEnd, в принципе для своих задач он подходит, вершин которые передаются
с CPU->GPU не так много как с 3D объектами.
Возможно когда нибудь и будет переделываться на буффер VBO`

Список файлов (актуально для версии 2.14):
| Файл | Описание |
| ------------- |:-------------|
| `GUIv2` | `Основная папка` |
| dlGUIFont.pas | Работа со шрифтами |
| dlGUIForm.pas | Форма со списком компонентов |
| dlGUIFormList.pas | Управление списком форм в т.ч. отображением их (FormList.Draw) |
| dlGUIObject.pas | Основной класс от которого наследованы компоненты и формы |
| dlGUIMouse.pas | Компонент для отображения курсора мыши |
| dlGUIPaletteHelper.pas | Модуль для работы с палитрой, по координатам определяет цвет в палитре |
| dlGUITypes.pas | Типы и классы которые используются в модулях |
| dlGUIVertexController.pas | Модуль для работы с вершинами в объектах (компонентах) |
| dlGUIXMLSerial.pas | Сериализация "Классов GUI" в формат XML |
| `components` | `Компоненты` |
| components\dlGUIBevel.pas | Рамка |
| components\dlGUICheckBox.pas | Чекбокс (переключатель) |
| components\dlGUIComboBox.pas | Раскрывающийся список |
| components\dlGUIEditBox.pas | Текстовое поле ввода (однострочное) |
| components\dlGUIImage.pas | Изображение |
| components\dlGUILabel.pas | Текст (может быть многострочным) |
| components\dlGUIListBox.pas | Список строк |
| components\dlGUIMainMenu.pas | Главное меню |
| components\dlGUIPanel.pas | Панель без компонентов, может быть заменено Bevel (рамкой) |
| components\dlGUIProgressBar.pas | Индикатор загрузки |
| components\dlGUIRadioButton.pas | Переключатели выбора |
| components\dlGUITable.pas | Таблица |
| components\dlGUITrackBar.pas | Выбор числового значения с помощью ползунка |
| components\dlGUITracker.pas | Трекер (горизонтальный/вертикальный) используется для компонентов таких как (ComboBox, ListBox, Table)... |
| `Textures` | `Модули для работы с текстурами` |
| Textures\dlTextureLoader.pas | Модуль для загрузки текстур формата (BMP, JPG, JPEG, TGA, PNG) |
| Textures\dlTextureList.pas | Управление текстурами (поиск, добавление, удаление) |
| `OpenGL` | `Модуль для работы с OpenGL ` |
| OpenGL\dlOpenGL.pas | Работа с фреймворком OpenGL |
| `Image` | `Список изображений` |
| Image\Logo.png | Логотип |
| Image\GUIPalette.bmp | Палитра для компонентов |
| Image\Consolas\ | Шрифт consolas |
| Images\Gadugi | Шрифт Gadugi |
| Images\Verdana | Шрифт Verdana |
---

Все компоненты унаследованы от класса dlGUIObject.TGUIObject
Для загрузки текстуры используются модули
- dlTextureLoader.pas
- dlTextureList.pas

Для начала использования GUI необходимо
- Создать контекст OpenGL (исп модуль dlOpenGL.pas)
- Загрузить текстуры (dlTextureLoader.pas)
- Создать форму (dlGUIForm.pas)
- Создать компоненты и добавить их на форму (components\...)
- Добавить форму в список форм (dlGUIFormList.pas)

Пример создания компонентов:
--
Форма:
```pascal
uses dlGUIForm;
...
var Form      : TGUIForm; //Класс формы
...
//Загрузка текстур
TextureList.Add('GUI', '.\Image\GUIPallette.bmp', GL_RGB, GL_MODULATE, GL_NEAREST, True, RGB(252, 52, 252));
TextureList.Add('GUIFont'   , '.\Image\Verdana\Verdana 8.fgl.png', GL_RGBA, GL_MODULATE, GL_LINEAR);
...
Form:= TGUIForm.Create('FormName', TextureList.Search('GUI')); //Создание класса формы
Form.Caption:= 'Заголовок формы';
Form.SetRect(0, 0, 200, 100); //Начальная позиция и размер формы
Form.Font.SetTextureLink(TextureList.Search('GUIFont')); //Установка шрифта формы
//Добавление формы в список форм
FormList.AddForm(Form);
... (render)

FormList.Draw;
```

`При добавлении на форму компонентов если у компонента не указана 
текстура и шрифт, то она назначается такой же как у формы`

Кнопка (Button):
```pascal
uses dlGUIButton;
...
var Button: TGUIButton;
...
Button:= TGUIButton.Create; //Создание класса кнопки
Button.SetRect(10, 10, 200, 10); //Позиция и размер кнопки
Button.Caption:= 'Текст кнопки'; //Текст
Button.OnClick:= Proc; //Вызываемая процедура

//Включить всплывающую подсказку
Button.Hint.Enable:= True;
Button.Hint.Text  := 'Текст всплывающей подсказки';

Button.Flat := False; // Скрыть/Показать рамку кнопки

//Если добавляем на форму
Form.AddComponent(Button);

//Если просто нужно отобразить
Button.Draw;
```

Поле ввода:
```pascal
uses dlGUIEditBox;
...
var EditBox: TGUIEditBox;
...
EditBox:= TGUIEditBox.Create;
EditBox.SetPos(10, 10, 200, 20);

Form.AddComponent(EditBox);

```



