# Tables (Board Game)
Данная программа содержит модули:
- AbstractData - описание типов 
- Constants - описание констант
- AdditionalFunction  - описание вспомогательных функций
- TablesBoardGame - основные функции приложения

Типы
=======
### Вспомогательные типы


`AuxCoord`: Используется для хранения координат.
Содержит два значения типа Float.

`AuxRect`: Используется для хранения координат прямоугольника.
Содержит четыре значения типа Float.

### Типы абстракции игры ***"Нарды"***
`AppStates`: Описание состояния игры:
1. Ход ***игрока 1***;
2. Ход ***игрока 2***;
3. Бросок костей ***игроком 1***;
4. Бросок костей ***игроком 2***.

`DAppDtState`: Тип описывающий все возможные значения используемые при работе приложения:
- Масштаб отрисовки изображения на экране по оси **X**;
- Масштаб отрисовки изображения на экране по оси **Y**;
- Данные ***игрока 1*** записанные типом **DPinf**;
- Данные ***игрока 2*** записанные типом **DPinf**;
- Данные ***игровой доски*** записанные типом **DGBoard**;
- ***Состояние игры*** с типом **AppStates**;
- Время с последнего обновления кадра;
- Цифра на первой кости;
- Цифра на второй кости.

`DChPoint`: Описание игрового пункта:    
- Координаты пункта в системе координат окна;
- Направление отрисовки пункта: вверх или вниз;
- Количество шашек в пункте;
- Номер пункта. Отсчет ведется от правого верхнего пункта против часовой стрелки.