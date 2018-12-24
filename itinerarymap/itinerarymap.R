data = read.csv("air_weather_geo.csv")
data = na.omit(data)
#View(data)
# 参考 https://zhuanlan.zhihu.com/p/38164684
library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(plyr) # 用于去重复记录
# (1)地理坐标
# 由于都是美国（包括出发地和目的地），所以对洲进行划分
# data.state是用sql对data进行筛选聚合操作后的数据
data.state = read.csv("state_geo.csv",col.names=c("state","lon","lat"),header=FALSE)
data.state$id = 1:51
data.state$catagory = c()
list1 = c("WA","OR","CA")
list2 = c('ID','MT','WY','NV','UT','CO')
list3 = c('AZ','NM','TX')
list4 = c('ND','MN','SD','IA','WI','IL')
list5 = c('NE','KS','MO','OK','AR','LA')
list6 = c('MI','IN','OH','KY','WV','VA')
list7 = c('TN','NC','MS','AL','GA','SC','FL')
list8 = c('VT','ME','NY','PA','NH','MA','RI','CT','NJ','DE','MD',"HI","DC")
list9 = c('AK')
# 对50个州按区域划分
for (i in 1:51){
  if (data.state$state[i] %in% list1) data.state$catagory[i]=1
  if (data.state$state[i] %in% list2) data.state$catagory[i]=2
  if (data.state$state[i] %in% list3) data.state$catagory[i]=3
  if (data.state$state[i] %in% list4) data.state$catagory[i]=4
  if (data.state$state[i] %in% list5) data.state$catagory[i]=5
  if (data.state$state[i] %in% list6) data.state$catagory[i]=6
  if (data.state$state[i] %in% list7) data.state$catagory[i]=7
  if (data.state$state[i] %in% list8) data.state$catagory[i]=8
  if (data.state$state[i] %in% list9) data.state$catagory[i]=9 
}

##（2）数据整合from to
# 2018年1-12月份的航线
data.fromto = subset(data,select = c("Month","state","state_arr"))
# 提取state和state_arr不是""的记录
data.fromto$state = as.vector(data.fromto$state)# 由于因子型会报错，转化为非因子型
data.fromto$state_arr = as.vector(data.fromto$state_arr)
data.fromto = data.fromto[-c(which(data.fromto$state==""),which(data.fromto$state_arr=="")),]
# levels(data.fromto$state)

# 以月份为参数，选择每个月份绘制航线图
data.plot <- data.fromto[which(data.fromto$Month==3),]
data.plot <- data.plot[,2:3]
colnames(data.plot) <- c("from","to")
n <- dim(data.plot)[1] # 用于计算边的权重
for(i in 1:n){
  data.plot$from[i]=data.state$id[which(data.state$state==data.plot$from[i])]
  data.plot$to[i]=data.state$id[which(data.state$state==data.plot$to[i])]
  # data.plot$categry[i]=data.state$catagory[which(data.state$id==data.plot$from[i])]
  }
data.plot = ddply(data.plot,.(from,to),nrow)  # 去掉重复记录
data.plot$V1 = data.plot$V1/n  # 计算边的权重
m = dim(data.plot)[1]  
for(i in 1:m){
  data.plot$categry[i]=data.state$catagory[which(data.state$id==data.plot$from[i])]
  data.plot$x[i]=data.state$lon[which(data.state$id==data.plot$from[i])]
  data.plot$y[i]=data.state$lat[which(data.state$id==data.plot$from[i])]
  data.plot$xend[i]=data.state$lon[which(data.state$id==data.plot$to[i])]
  data.plot$yend[i]=data.state$lat[which(data.state$id==data.plot$to[i])]
  }
colnames(data.plot) = c("from","to","weight","category","x","y","xend","yend")  # 重命名
#   id    lon    lat    state
nodes = subset(data.state,select = c('id','lon','lat','state'))
#  from   to    weight  category
edges = data.plot
edges <- edges %>% mutate(category = as.factor(category))
g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
# 对每个节点赋予权重，并使用等级作为指标，体现为节点的大小
nodes$weight = degree(g)
# 定义主题，没有坐标轴和网格线
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
# 所有的图应用同一个主题，并用相同的世界地图作为背景

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                             data = map_data('world'),
                             fill = "#CECECE", color = "#515151",
                             size = 0.15)# 固定比例坐标系限制经纬度
mapcoords <- coord_fixed(xlim = c(-156,-66), ylim = c(19,60))
# 绘图
edges = edges[-which(edges$from==edges$to),]
ggplot(nodes) + country_shapes +
  # 将节点连成曲线
  geom_curve(aes(x = edges$x, y = edges$y, xend = edges$xend, yend = edges$yend,
                 color = edges$category, size = edges$weight),
             data = edges, curvature = 0.33,
             alpha = 0.5,show.legend=F) +
  # 连线的宽度
  scale_size_continuous(guide = FALSE, range = c(0.25, 5)) +
  # geom_point用于绘制节点
  # 把lon,lat映射到地图上
  geom_point(aes(x = nodes$lon, y = nodes$lat),              
             shape = 21, size = 3, fill = 'white',
             color = 'black', stroke = 0.5) +
  # geom_text用于添加标签
  geom_text(aes(x = nodes$lon, y = nodes$lat, label = nodes$state),
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 3, color = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5,size = 20),plot.subtitle =element_text(hjust = 0.65,size = 15))+
  labs(title="美国九大区域航线分布图",subtitle = "Jan,2008")+
  mapcoords + maptheme
#####################################
# gif动图
frames2gif <- function(pathIn='',# pathIn: 图片所在路径
                       pathOut='',# pathOut: 生成的gif所保存的路径
                       ImageMagick_path='',# ImageMagick_path: ImageMagick convert命令所在路径
                       resize_ratio=1,# resize_ratio: 调节gif的尺寸，默认为1。如果为0.5，gif的长度和宽度将是图片尺寸的一半
                       delay=40,# delay: 设置帧与帧之间的时间间隔，默认为40(表示0.4s)。如果为200,那么时间间隔即为2s
                       frameFormat='png',# frameFormat: 图片的格式
                       everyFrame=1){# everyFrame: 如果为3，只使用pathIn文件夹下的第1, 4, 7帧，..图片来制作gif，默认使用所有图片
  ## create temp dir to store frames used to create gif.
  tempdir <- paste0(pathIn, '/temp')
  dir.create(tempdir)
  files <- list.files(pathIn, pattern=paste0('*.', frameFormat), recursive=FALSE, full.names=TRUE)
  index <- seq(1, length(files), by=everyFrame)
  file.copy(files[index], tempdir)
  command <- paste(ImageMagick_path,
                   '-resize', paste0(as.integer(100L*resize_ratio), '%'),
                   '-delay', delay, 
                   paste0(tempdir,'/*.', frameFormat),
                   pathOut)
  #system('F:R_tutorials/gif/ImageMagick-7.0.8-64bit/convert -resize 90% -delay 40 *.png result.gif')
  system(command)
  ## delete temp dir
  unlink(tempdir, recursive=TRUE, force=TRUE)
}
############
pathIn <- 'picture'
pathOut <- 'picture_out/fast.gif'
ImageMagick_path <- 'G:/ImageMagic/ImageMagick-7.0.8-Q16/convert'
# 输出结果
frames2gif(pathIn, pathOut, 
           ImageMagick_path, 
           delay=90)
