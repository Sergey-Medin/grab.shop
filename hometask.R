################################################################################
#                                                                              #
# Скрипт для выгрузки данных из интернет магазина www.willsmart.ru             #
# Результат выгрузки сохраняется в каталог ./output/ в формате CSV             #
#                                                                              #
# Автор: Сергей Мединцев / sergey.medintsev@ya.ru                              #
# Дата:  16.02.16                                                              #
#                                                                              #
################################################################################

options(stringsAsFactors = FALSE)
require(rvest)

# ====== ИНИЦИАЛИЗАЦИЯ ПЕРЕМЕННЫХ ======
# адрес каталога, из которого будет производится выгрузка данных
catalog.url <- 'http://www.willsmart.ru/catalog/house-technics/chayniki_elektricheskie'
# catalog.url <- 'http://www.willsmart.ru/catalog/electronics/dok_stantsii_dlya_smartfonov_planshetov_pleerov'
# catalog.url <- 'http://www.willsmart.ru/catalog/house-technics/uvlazhniteli_1'
# catalog.url <- 'http://www.willsmart.ru/catalog/house-technics/zubnye_shchetki'

# название каталога, будет использовано при сохранении результатов
catalog.name <- tail(strsplit(catalog.url, split = '/')[[1]], 1)

# путь до файла, в который будут сохраняться результаты
file4result <- paste0('output/', catalog.name, '.csv')

# товары в интернет магазине www.willsmart.ru выводятся постранично
page.number        <- 0       # номер обрабатываемой страницы
product.link.first <- NA      # ссылка на первый товар
                              #   (нужна для остановки выгрузки, см. ниже)
PagesNotFinished   <- TRUE    # булевый флаг означающий закончились ли страницы
                              #   с товарами
product.data.list  <- list()  # список dataframe'ов, каждый из которых хранит
                              #   информацию о конкретном товаре

# ====== ЗАГРУЗКА ДАННЫХ С САЙТА ======
# перебор всех страниц с товарами
while (PagesNotFinished){
  page.number <- page.number + 1
  
  catalog.link <- paste0(catalog.url, '?PAGEN_1=', page.number)
  catalog.page <- read_html(catalog.link)
  
  # найден ли первый div содержащий информацию о товаре
  first.found <- FALSE
  
  # перебор всех товаров на странице
  product.id <- 0
  while(TRUE){
    
    product.id <- product.id + 1 
    
    product.link <- catalog.page %>%
      html_nodes(xpath=paste0('//*[@id="catalog"]/div[', product.id, ']/div[2]/h4/a')) %>%
      html_attrs
    
    if(length(product.link) > 0){
      product.link <- paste0('http://www.willsmart.ru', product.link[[1]]['href'])
      first.found <- TRUE
    } else {
      product.link <- NA
    }
    
    # условия для перехода к следующему div или завершения обаботки страницы
    #   с товарами
    if(!first.found & is.na(product.link)){
      next
    } else {
      if(first.found & is.na(product.link)){
        break
      }
    }
    
    # условия для задания ссылки на первый товар и завершения выгрузки данных
    #   с сайта
    if (is.na(product.link.first)){
      product.link.first <- product.link
    } else {
      if(product.link == product.link.first){
        PagesNotFinished <- FALSE
        break
      }
    }
    
    # получение названия товара
    product.name <- catalog.page %>%
      html_nodes(xpath=paste0('//*[@id="catalog"]/div[', product.id, ']/div[2]/h4/a')) %>%
      html_text
    product.name <- iconv(x = product.name, from = 'UTF-8')
    
    # получение цены товара
    product.price <- catalog.page %>%
      html_nodes(xpath=paste0('//*[@id="catalog"]/div[', product.id, ']/div[2]/div[3]/div/div[1]/span')) %>%
      html_text
    product.price <- gsub(pattern = '[^0-9]', replacement = '', x = product.price)

    # вывод информации об обрабатываемом продукте
    print(paste(page.number, product.name, product.price))
    
    # загрузка детальной информации о продукте
    product.page <- read_html(product.link)
    
    product.data <- product.page %>%
      html_nodes(xpath='//*[@id="specification-content"]/table') %>%
      html_table
    
    product.data <- rbind(apply(X = product.data[[1]], MARGIN = 2, function(x){iconv(x = x, from = 'UTF-8')}),
                          c('Цена, (р)', product.price))
    product.data <- data.frame(product.data)
    colnames(product.data) <- c('feature', product.name)
    
    product.data.list[[length(product.data.list)+1]] <- product.data
 
  }
}

# ====== ФОРМИРОВАНИЕ ТАБЛИЦЫ РЕЗУЛЬТАТОВ И СОХРАНЕНИЕ ЕЁ В CSV файл ======
result <- Reduce(function(x,y){merge(x = x, y = y,
                                     by = 'feature', all = TRUE)},
                 product.data.list)
rownames(result) <- result$feature
result$feature <- NULL
result <- as.data.frame(t(result))

write.csv(result, file = file4result, fileEncoding = "UTF-8")

