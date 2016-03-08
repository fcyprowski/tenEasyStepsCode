file = '/home/filip/tenEasySteps.xlsx'

createMainTextFrame = function(file, klatki_per_zdanie=5){
  require(xlsx)
  teks = read.xlsx(file, sheetIndex=1)
  
  ## make it longer to get the smooth animation
  splited = split(teks, teks$number)
  splited2 = lapply(splited, function(x){
    rows = list()
    for(i in seq_along(x$tekst)){
      row = data.frame(tekst=rep(x$tekst[i], klatki_per_zdanie),
                       number=x$number[i],
                       stringsAsFactors=F)
      rows[[i]] = row
    }
    rows_bind = do.call('rbind', rows)
  })
  mainTextFrame = do.call('rbind', splited2)
  mainTextFrame$x = 50
  mainTextFrame$y = 50
  mainTextFrame$x_sec = 1:nrow(mainTextFrame)
  mainTextFrame$y_sec = 1:nrow(mainTextFrame)
  mainTextFrame$tekst = as.character(mainTextFrame$tekst)
  mainTextFrame$tekst[mainTextFrame$number==0] = toupper(mainTextFrame$tekst[mainTextFrame$number==0])
  mainTextFrame$tekst[is.na(mainTextFrame$tekst)] = ''
  mainTextFrame$tekst = gsub('\\\\N', '\n', mainTextFrame$tekst)
  return(mainTextFrame)
}

sideTexts = function(file_skrot, dir='/home/filip/'){
  files = dir(dir)
  files2 = files[grep(file_skrot, files)]
  file_list = list()
  for(i in files2){
    index = as.numeric(str_extract(pattern='[0-9]+', string=i))
    file_list[[index]] = zlozTekst(i)
  }
  sideText = file_list
  return(sideText)
}

zlozTekst = function(file){
  f = readLines(file)
  if(length(f)>1){
    f = paste(f, collapse='\n')
  }
  return(f)
}

addColors = function(mainTextFrame, colorfile='/home/filip/tenEasyStepsColors'){
  cols = readLines(colorfile)
  cols_vec = rep(cols, nrow(mainTextFrame))[1:nrow(mainTextFrame)]
  mainTextFrame$col = paste('#', cols_vec, sep='')
  return(mainTextFrame)
}

addAdditionalTexts = function(mainTextFrame, sideText){
  mainTextFrame$sidetext = ''
  for(i in seq_along(sideText)){
    mainTextFrame$sidetext[mainTextFrame$number==i] = sideText[[i]] 
  }
  return(mainTextFrame)
}

plotTextFrame = function(mainTextFrame, directory){
  require(ggplot2)
  for(i in seq_along(mainTextFrame$tekst)){
    themeeasy = theme(plot.background=element_rect(color=mainTextFrame$col[i],
                                                   fill=mainTextFrame$col[i]),
                        panel.grid=element_blank(),
                        axis.text.y=element_blank(),
                        axis.text.x=element_blank(),
                        panel.background=element_rect(fill=mainTextFrame$col[i]),
                        axis.ticks=element_blank(),
                        plot.title=element_text(size=title_size, color=title_color))
    g = ggplot()+geom_point(data=mainTextFrame, aes(x=x_sec, y=y_sec), color=mainTextFrame$col[i])+
      geom_text(data=mainTextFrame, x=nrow(mainTextFrame)/2, 
                y=nrow(mainTextFrame)/2, label=mainTextFrame$tekst[i],
                family='Apolonia', size=19, color='white')
      themeeasy+
      xlab('')+ylab('')
    if(i<10){
      name = paste('a',i)
    }else if(i<100){
      name = paste('b', i)
    }else{
      name = paste('c', i)
    }
    print(name)
    png(paste(directory, '/', name, '.png', sep=''), 850, 710)
    print(g)
    dev.off()
  }
}

createSpecialGeomText = funtion(i, number, mainTextFrame){
  geom = geom_text(data=mainTextFrame, aes(x=sample(60:80, 1)), y=sample(60:80, 1))
}

makeGif = function(directory, delay, filename){
  expression = paste("convert -delay ", delay, ' ', directory, '*.png ', 
                     directory, filename, ".gif", sep='')
  print(expression)
  system(expression)
}

