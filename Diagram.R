library(DiagrammeR)

diagram <- grViz("digraph {
  
graph[layout = dot, fontname = 'Ubuntu', fontsize = 18]

subgraph cluster_a {
  rank = same;
  node[shape = box, width = 2.5, height = .5, fixedsize = True]
  D1a[label='Реестр МСП\n(N zip-архивов)']
  D2a[label='Извлечённые данные\n(N csv-файлов)']
  D3a[label='Объединённая таблица\n(csv-файл)']
  D4a[label='Геопривязанная таблица\n(csv-файл)']
  
  node[shape = none, width = 2, height = .5, fixedsize = True]
  A2a[label = 'Извлечение, фильтрация,\nтрансформация (2а)']
  A3a[label = 'Агрегирование (3а)']
  A4a[label = 'Геопривязка (4)']
  
  D1a -> A2a -> D2a -> A3a -> D3a -> A4a -> D4a
}

subgraph cluster_b {
  rank = same;
  node[shape = box, width = 2.5, height = .5, fixedsize = True]
  D1b[label='Доходах и расходы\n(M zip-архивов)']
  D2b[label='Извлечённые данные\n(M csv-файлов)']
  D4b[label='Отфильтрованная таблица\n(csv-файл)']
  
  node[shape = none, width = 2, height = .5, fixedsize = True]
  A2b[label = 'Извлечение и трансформация (2б)']
  A3b[label = 'Агрегирование и фильтрация(3б)']
  
  D1b -> A2b -> D2b -> A3b -> D4b
}

subgraph cluster_c {
  rank = min;
  node[shape = box, width = 2.5, height = .5, fixedsize = True]
  D1c[label='Число работников\n(K zip-архивов)']
  D2c[label='Извлечённые данные\n(K csv-файлов)']
  D4c[label='Отфильтрованная таблица\n(csv-файл)']
  
  node[shape = none, width = 2, height = .5, fixedsize = True]
  A2c[label = 'Извлечение, трансформация (2в)']
  A3c[label = 'Агрегирование и фильтрация (3в)']
  
  D1c -> A2c -> D2c -> A3c -> D4c
}

# nodes
# D stands for 'data', A stands for 'action'
node[shape = box, width = 3, height = .5, fixedsize = True]
Source[label='Открытые данные ФНС России']

D5[label = 'Панельное представление']

D_aux[label = 'Справочник\nнаселённых пунктов']

node[shape = none, width = 2, height = .5, fixedsize = True]
A1[label = 'Загрузка (1)']
A5[label = 'Объединение и трансформация (5)']

Source -> A1 -> {D1a, D1b, D1c}
D_aux -> A4a
D3a -> {A3b, A3c} 
{D4a, D4b, D4c} -> A5 -> D5 
}")

