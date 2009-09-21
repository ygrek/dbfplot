class en = object
method open_file = "Open file"
method quit = "Quit"
method file = "File"
end

class ru = object 
inherit en
method open_file = "Выбрать файл"
method draw = "Построить график"
method quit = "Выйти"
method check_valid = "Отметить все непустые"
method clear_all = "Снять все отметки"
method single_plot = "В одной сетке"
method show_mouse_values = "Показывать значения под курсором"
method file = "файл"
method save_img = "Сохранить график"
end
