#.libPaths("/mnt/ilustre/app/pub/R/lib64/R/library")
#.libPaths("/mnt/ilustre/users/fengbo.zeng/R/x86_64-unknown-linux-gnu-library/3.2")
#bbp::bbp()
library(shiny)
library(yaml)
library(RMySQL)
library(RcppRoll)
library(magrittr)
library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(yaml)
library(rCharts)
##########readfile######
file<-"/mnt/ilustre/users/danni.li/NIPT/WS-170097_R1.bed.2"
 x = fread(file, header = F) %>%
    setnames(c("V1","V2","V3","V4","V5","V6","V7","V8"),
             c("chr","start","end","gc","map","pn","reads","gid"))
  x$gid = as.character(x$gid)
  fread.bed=x
  
 ######filter data######
   map.min = 0.9
   samplesize = 50000
   verbose = T
   gc.cor = T
   map.cor = T
   if (length(x$reads) == 0 | length(x$gc) == 0 | length(x$map) == 0)
    stop("Missing one of required columns: reads, gc, map")
  x = data.table(x)
  x[, valid:=(reads>0 & gc > 0)]
  reads.range <- x[, quantile(reads[valid], prob = c(0.01, 1-0.01), na.rm = TRUE)]
  gc.range <- x[, quantile(gc[valid], prob = c(0.01, 1 - 0.01), na.rm = TRUE)]
  x[, ideal:=(map > map.min &
               reads > reads.range[1] & reads < reads.range[2] &
               chr <= 22 &
               gc > gc.range[1] & gc < gc.range[2] &
               pn == 0)]
#######loess and lowess#######
  set <- which(x$ideal)
  select <- sample(set, min(length(set), samplesize)) ####取不多于50000
  rough <- loess(x$reads[select] ~ x$gc[select], span = 0.03)
  i <- seq(0, 1, by = 0.001)
  final <- loess(predict(rough, i) ~ i, span = 0.3)
   x[reads != 0 & gc != 0, cor.gc := reads/predict(final, gc)]

  coutlier <- 0.001
  range <- quantile(x$cor.gc[which(x$ideal)], prob = c(coutlier, 1 - coutlier), na.rm = TRUE)
  set <- which(x$cor.gc < range[2] & x$cor.gc > range[1])
  select <- sample(set, min(length(set), samplesize))
  final <- approxfun(lowess(x$map[select], x$cor.gc[select]))
 cor.cn = x[, cor.map := cor.gc/final(map)][,copy:=map.cor*cor.map+(!map.cor)*cor.gc]
  ############bin##########
  bin.cn <- function(x, bin.step=1*10^6, bin.window=bin.step){
  message("run bin.cn")
  #x = data.table(x)
  x = data.table(x)[map>0.8,]
  x = x[, bin:= start %/% bin.step+1][
    , .(cn1=mean(copy, na.rm = T)), by=.(chr,bin)]
  cn1.mean = x[chr %in% c(1:12,14:17,19:20,22),][cn1 > quantile(cn1,0.05,na.rm = T) & cn1 < quantile(cn1,0.95,na.rm = T),mean(cn1)]
  x[,cn1:=cn1/cn1.mean]

  n <- bin.window %/% bin.step
  x.1 = x[chr!=25][
    , .(cn=roll_mean(cn1, n, na.rm = T)), by=.(chr)][
    , bin:=seq_along(cn), by=.(chr)][
    , n:=seq_along(cn),
   ]
  x.2 = x[chr==25][
      , .(cn=cn1), by=.(chr)][
      , bin:=seq_along(cn), by=.(chr)][
      , n:=seq_along(cn)+max(x.1$n),
   ]
  list(x.1, x.2) %>% rbindlist(use.names=T)
}
  ########z#####
  test.cn=cor.cn
 load("/mnt/ilustre/users/fengbo.zeng/app/nipt/data/1.ref.cor.Rdata")
  ref.cn=x
   bs=1*10^6
   bw=10*10^6
    message("ref.gids = ", paste(ref.cn$gid %>% unique(), collapse=","))
  message("test.gids = ", paste(test.cn$gid %>% unique(), collapse=","))
  ref.binned.cns = data.table(ref.cn)[, bin.cn(.SD,bs,bw), by=gid]
  test.binned.cns = data.table(test.cn)[, bin.cn(.SD,bs,bw), by=gid]

  ref = ref.binned.cns[, .(sd=sd(cn,na.rm = T), mean=mean(cn, na.rm = T)),
                       by=.(chr, bin)]
  z = test.binned.cns %>%
    left_join(ref, by=c("chr","bin")) %>%
    group_by(gid) %>%
    mutate(z=(cn-mean)/sd,n=seq_along(cn))
    z.cn <-z
    zs.out<-z
    zs <-zs.out
    write.table(zs.out, file = "z.xls",row.names = F, quote = F, sep = "\t")
 
#########zplot#######
   view.cns=zs
   z.ylim=30
   height=800
   width=1200
  ##ggplot##
    message("run plot.cn")
 p= ggplot(view.cns, aes(n,z)) +
    geom_line() +
    geom_point(aes(colour=as.factor(chr %% 2))) +
    facet_grid(gid~.) +
    ylim(-z.ylim, z.ylim)
ggsave(p,file="plot.pdf",width=12,height=4)
  ###rplot###
	view.cns = data.table(view.cns)
    view.cns[, group := chr %% 2][is.na(z), z:=0][z > z.ylim, z:=z.ylim][z < -z.ylim, z := -z.ylim]
    p = rPlot(z ~ n | gid, data = view.cns,
            tooltip="#!function(iterm){return ('chr:'+iterm.chr+', bin:'+ iterm.bin + ', z:' + iterm.z)}!#",
            type = 'point', size=list(const=2),
            facet='gid', color='group')
    p$layer(z ~ n | gid, data = view.cns,
          size = list(const = 0.5),
          type = 'line',
          color=list(const="black"),
          copy_layer = T
    )
    p$params$height = height
    p$params$width = width
    p$addParams(dom = 'plot')
    p$facet(cols = 1)
    p
	p$save('p.html')
	
    #cat('<iframe src="mplot.html"width=100%,height=40></iframe>')
	
