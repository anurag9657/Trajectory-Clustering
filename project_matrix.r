library("dplyr")

setwd("C:/Users/pjung1/Dropbox/Class/03 Doctor/2017-01 Spring/ITCS 8156 ML/04 project")

d.20.35 <- read.csv("sample_output/new parameters/segs0.02_35.csv", header=T, sep=" ")
d.20.45 <- read.csv("sample_output/new parameters/segs0.02_45.csv", header=T, sep=" ")
d.20.55 <- read.csv("sample_output/new parameters/segs0.02_55.csv", header=T, sep=" ")
d.25.35 <- read.csv("sample_output/new parameters/segs0.025_35.csv", header=T, sep=" ")
d.25.45 <- read.csv("sample_output/new parameters/segs0.025_45.csv", header=T, sep=" ")
d.25.55 <- read.csv("sample_output/new parameters/segs0.025_55.csv", header=T, sep=" ")
d.30.35 <- read.csv("sample_output/new parameters/segs0.03_35.csv", header=T, sep=" ")
d.30.45 <- read.csv("sample_output/new parameters/segs0.03_45.csv", header=T, sep=" ")
d.30.55 <- read.csv("sample_output/new parameters/segs0.03_55.csv", header=T, sep=" ")
d.35.35 <- read.csv("sample_output/new parameters/segs0.035_35.csv", header=T, sep=" ")
d.35.45 <- read.csv("sample_output/new parameters/segs0.035_45.csv", header=T, sep=" ")
d.35.55 <- read.csv("sample_output/new parameters/segs0.035_55.csv", header=T, sep=" ")

d.20.35 <- d.20.35 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.20.45 <- d.20.45 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.20.55 <- d.20.55 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.25.35 <- d.25.35 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.25.45 <- d.25.45 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.25.55 <- d.25.55 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.30.35 <- d.30.35 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.30.45 <- d.30.45 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.30.55 <- d.30.55 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.35.35 <- d.35.35 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.35.45 <- d.35.45 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)
d.35.55 <- d.35.55 %>% rename(tr=trajectory_index, cl=cluster) %>% mutate(ID=1:2118)

d2.20.35 <- d.20.35 %>% filter(cl>0)
d2.20.45 <- d.20.45 %>% filter(cl>0)
d2.20.55 <- d.20.55 %>% filter(cl>0)
d2.25.35 <- d.25.35 %>% filter(cl>0)
d2.25.45 <- d.25.45 %>% filter(cl>0)
d2.25.55 <- d.25.55 %>% filter(cl>0)
d2.30.35 <- d.30.35 %>% filter(cl>0)
d2.30.45 <- d.30.45 %>% filter(cl>0)
d2.30.55 <- d.30.55 %>% filter(cl>0)
d2.35.35 <- d.35.35 %>% filter(cl>0)
d2.35.45 <- d.35.45 %>% filter(cl>0)
d2.35.55 <- d.35.55 %>% filter(cl>0)

library("tnet")

net.20.35 <- projecting_tm(d2.20.35 %>% select(ID, cl), method="binary")
net.20.45 <- projecting_tm(d2.20.45 %>% select(ID, cl), method="binary")
net.20.55 <- projecting_tm(d2.20.55 %>% select(ID, cl), method="binary")
net.25.35 <- projecting_tm(d2.25.35 %>% select(ID, cl), method="binary")
net.25.45 <- projecting_tm(d2.25.45 %>% select(ID, cl), method="binary")
net.25.55 <- projecting_tm(d2.25.55 %>% select(ID, cl), method="binary")
net.30.35 <- projecting_tm(d2.30.35 %>% select(ID, cl), method="binary")
net.30.45 <- projecting_tm(d2.30.45 %>% select(ID, cl), method="binary")
net.30.55 <- projecting_tm(d2.30.55 %>% select(ID, cl), method="binary")
net.35.35 <- projecting_tm(d2.35.35 %>% select(ID, cl), method="binary")
net.35.45 <- projecting_tm(d2.35.45 %>% select(ID, cl), method="binary")
net.35.55 <- projecting_tm(d2.35.55 %>% select(ID, cl), method="binary")

library("igraph")
g.20.35 <- graph_from_data_frame(net.20.35, directed=T, vertices=d.20.35 %>% select(ID))
g.20.45 <- graph_from_data_frame(net.20.45, directed=T, vertices=d.20.45 %>% select(ID))
g.20.55 <- graph_from_data_frame(net.20.55, directed=T, vertices=d.20.55 %>% select(ID))
g.25.35 <- graph_from_data_frame(net.25.35, directed=T, vertices=d.25.35 %>% select(ID))
g.25.45 <- graph_from_data_frame(net.25.45, directed=T, vertices=d.25.45 %>% select(ID))
g.25.55 <- graph_from_data_frame(net.25.55, directed=T, vertices=d.25.55 %>% select(ID))
g.30.35 <- graph_from_data_frame(net.30.35, directed=T, vertices=d.30.35 %>% select(ID))
g.30.45 <- graph_from_data_frame(net.30.45, directed=T, vertices=d.30.45 %>% select(ID))
g.30.55 <- graph_from_data_frame(net.30.55, directed=T, vertices=d.30.55 %>% select(ID))
g.35.35 <- graph_from_data_frame(net.35.35, directed=T, vertices=d.35.35 %>% select(ID))
g.35.45 <- graph_from_data_frame(net.35.45, directed=T, vertices=d.35.45 %>% select(ID))
g.35.55 <- graph_from_data_frame(net.35.55, directed=T, vertices=d.35.55 %>% select(ID))

M.20.35 <- as.matrix(as_adjacency_matrix(g.20.35), type="both")
M.20.45 <- as.matrix(as_adjacency_matrix(g.20.45), type="both")
M.20.55 <- as.matrix(as_adjacency_matrix(g.20.55), type="both")
M.25.35 <- as.matrix(as_adjacency_matrix(g.25.35), type="both")
M.25.45 <- as.matrix(as_adjacency_matrix(g.25.45), type="both")
M.25.55 <- as.matrix(as_adjacency_matrix(g.25.55), type="both")
M.30.35 <- as.matrix(as_adjacency_matrix(g.30.35), type="both")
M.30.45 <- as.matrix(as_adjacency_matrix(g.30.45), type="both")
M.30.55 <- as.matrix(as_adjacency_matrix(g.30.55), type="both")
M.35.35 <- as.matrix(as_adjacency_matrix(g.35.35), type="both")
M.35.45 <- as.matrix(as_adjacency_matrix(g.35.45), type="both")
M.35.55 <- as.matrix(as_adjacency_matrix(g.35.55), type="both")

M <- M.20.35 +
  M.20.45 +
  M.20.55 +
  M.25.35 +
  M.25.45 +
  M.25.55 +
  M.30.35 +
  M.30.45 +
  M.30.55 +
  M.35.35 +
  M.35.45 +
  M.35.55

g <- graph_from_adjacency_matrix(M, mode=c("undirected"), weighted=T)
g <- g %>% delete.vertices(v=which(degree(g)==0))
g2 <- subgraph.edges(g, eids=which(E(g)$weight>8))
g2 <- g2 %>% delete.vertices(v=which(degree(g2)==0))

cliques(g2, min=50)

