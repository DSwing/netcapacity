# library('tibble')
# library('ggplot2')
# library('svglite')
# library('tidyr')
# library('dplyr')
# library('tidygraph')
# library('ggraph')
# library('stringr')
# library('purrr')
# library('sf')
# library('showtext')
# library('magick')
# library('ggtext')
# library('lwgeom')
# library('sysfonts')
# library('arcgisbinding')


#' Read an online hosted feature layer
#'
#' @param path The path of the online hosted feature layer
#' @param precision The precision as per sf objects. Default 100 (greater values get better precision)
#'
#' @return sflayer The layer in sf format
#' @export
#' @examples
#' net_arcread("https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/HALO_BLG_1_Feeder_Cable_Route/FeatureServer")
net_arcread <- function(path, precision=100){
  arcgisbinding::arc.check_product()
  layers_names <- arcgisbinding::arc.open(path)@children$FeatureClass
  layers_paths <- sapply(layers_names, \(x) paste0(path,'/', x))
  layers <- list()
  for (i in seq_along(layers_paths)){
     try(assign (layers_names[i] , arcgisbinding::arc.data2sf(arcgisbinding::arc.select(arcgisbinding::arc.open(layers_paths[i])))))
     layers[[i]] <- eval(as.name(layers_names[i]))}
  names(layers) <- layers_names
  for (i in seq_along(layers)){
     sf::st_precision(layers[[i]]$geom) <- precision}
  return (layers)
}


#' Read a layer from an Arcgis tool input
#'
#' @param x the list of layers in the param_in of the toolbox
#' @param precision The precision as per sf objects. Default 100 (greater values get better precision)
#'
#' @return list of the tool input layers as sf objects
#' @export
#' @examples
#' net_arcgisread(x)
net_arcgisread <- function(x, precision=100){
  data <- lapply(x, \(y) arc.data2sf(arc.select(arc.open(y))))
  for (i in seq_along(data)){
    sf::st_precision(data[[i]]$geom) <- precision}
  return (data)
}

#' Filter the net (wrapper around tidygraph to_subgraph())
#'
#' @param net Network graph
#' @param premises Premises layer in sf format
#' @param ... Parameter to be passed to filter function
#'
#' @return graph subNetwork as tidygraph graph
net_filter<- function(net, ...){
  tidygraph::convert(net,tidygraph::to_subgraph, ..., subset_by='nodes')
}

#' Search for nodes that shares the same geometries
#'
#' @param net Network graph
#'
#' @return list of multiple nodes indexes
net_multinodes <- function(net){
  net_nodes2sf(net) |> sf::st_equals() |> (\(x) x[sapply(x, \(y)(length(y)>1))])()
}



#' Convert net nodes to sf
#'
#' @param net Network graph
#'
#' @return sf rapresentation of the nodes
net_nodes2sf <- function(net){
  net_nodes2tibble(net) |> sf::st_as_sf()}

#' Convert net nodes to tibble
#'
#' @param net Network graph
#'
#' @return tibble rapresentation of the nodes
net_nodes2tibble <- function(net){
  net |> tidygraph::activate(nodes) |> tibble::as_tibble()}


#' Convert net edges to tibble
#'
#' @param net Network graph
#'
#' @return tibble rapresentation of the edges
net_edges2tibble <- function(net){
  net |> tidygraph::activate(edges) |> tibble::as_tibble()}


#' Create Network graph from layers as inputs
#'
#' @param cables Cables layer in sf format
#' @param closures Closures layer in sf format
#' @param directed TRUE if you want a directed graph(default)
#' @param cables_as_nodes if TRUE the cables are represented as nodes along with the devices, if FALSE(default) the cables are represented as edges
#'
#' @export
#' @return graph Network as tidygraph graph (the nodes table has a group column for connected subgraphs)
net_graph <- function(cables,closures, directed=TRUE, cables_as_nodes=FALSE)
{
  closures <- closures[,c("OBJECTID","label","label_2","status","owner","enc_type","enc_size","placement","sp1_type","sp1_label","sp2_type","sp2_label","sp3_type","sp3_label","reel_end","GlobalID")]
  cables <- cables[,c("OBJECTID","status","owner","cab_size","cab_type","placement","ref","prem_id","loc","GlobalID","use_","Shape__Length")]

  closures <- dplyr::mutate(closures, status=as.integer(status),owner=as.integer(owner), placement=as.integer(placement),node_type='device')
  closures2 <- subset(closures,(label_2 != " " & !is.na(label_2)))
  closures2 <- dplyr::mutate(closures2,label=label_2)
  nodes <- dplyr::bind_rows(closures, closures2)
  nodes$name <- nodes$label
  networkcables <- subset(cables, cab_size!='7')
  networkcables  <-  dplyr::mutate(networkcables, status=as.integer(status),owner=as.integer(owner), placement=as.integer(placement), node_type='cable')
  tocheckcables <- dplyr::mutate(subset(networkcables,!grepl('/',ref)), note='ref not standard')
  networkcables <- subset(networkcables,grepl('/',ref))
  networkcables <- dplyr::mutate(dplyr::rowwise(networkcables),from=strsplit(ref,'/')[[1]][[1]], to=strsplit(ref,'/')[[1]][[2]])
  tocheckcables <- rbind(tocheckcables,dplyr::mutate(subset(networkcables,!((from %in% nodes$name) & (to %in% nodes$name))), note='device corrispondence not found'))
  networkcables <- subset(networkcables,((from %in% nodes$name) & (to %in% nodes$name)))

  if(cables_as_nodes){

    nodes <- dplyr::bind_rows(networkcables,nodes)
    nodes <-  dplyr::mutate(nodes,name=ifelse(node_type=="cable", ref, label))
    nodes <- unique.data.frame(nodes)

    temp <- dplyr::filter(nodes, node_type=="cable")
    from <- c(temp$ref,temp$from)
    to<-c(temp$to,temp$ref)

    edges<-tibble::tibble(from=from, to=to)# |> subset(((from %in% nodes$name) & (to %in% nodes$name)))
  } else {
    edges <- networkcables
  }

  graph <- tidygraph::tbl_graph(nodes = nodes, edges = tibble::as_tibble(edges), node_key = "name", directed = directed) |>
             dplyr::mutate(group = tidygraph::group_components(type = "weak"))

  return (graph)
}


#' Take the splice closure sf and mutate it to include the capacity
#'
#' @param x = closures layer in sf format
#'
#' @export
#' @return sf of devices(pivot longer of splice closures)
net_devices <- function(x){
  encsizemap1=c('1'=4, '2'=8, '3'=8, '4'=8,'5'=8,'6'=12,'7'=2, '8'=12,'9'=24, '10'=4,'11'=8,'12'=12,'13'=12,'14'=12,'15'=12,'16'=32,'17'=64,'18'=96, '19'=288,'20'=32,'21'=64, '22'=8,'23'=4,'24'=8,'25'=32)
  encsizemap2=c('1'=0, '2'=0, '3'=4, '4'=8,'5'=0,'6'=0,'7'=0, '8'=0,'9'=0, '10'=0,'11'=0,'12'=0,'13'=4,'14'=8,'15'=12,'16'=0,'17'=0,'18'=0, '19'=0,'20'=0,'21'=0, '22'=12,'23'=0,'24'=0,'25'=0)
  encsizestring1=c('1'='1 x 1:4', '2'= '2 x 1:4', '3'='1 x 1:8', '4'='1 x 1:8','5'='1 x 1:8','6'='3 x 1:4','7'='', '8'='','9'='', '10'='4 Ports','11'='8 Ports','12'='12 Ports','13'='12 Ports','14'='12 Ports','15'='12 Ports','16'='1 x 1:32','17'='2 x 1:32','18'='3 x 1:32', '19'='','20'='1 x 1:32','21'='2 x 1:32', '22'='1 x 1:8','23'='1 x 1:4','24'='2 x 1:4','25'='4 x 1:8')
  encsizestring2=c('1'='', '2'='', '3'='1 x 1:4', '4'='2 x 1:4','5'='','6'='','7'='', '8'='','9'='', '10'='','11'='','12'='','13'='4 Ports','14'='8 Ports','15'='12 Ports','16'='','17'='','18'='', '19'='','20'='','21'='', '22'='3 x 1:4','23'='','24'='','25'='')
  x <-  tidyr::pivot_longer(x,c(label, label_2), names_to='lab', values_to='name') |>
    subset(!is.na(name) & name != ' ') |>
      dplyr::mutate(Nout = dplyr::case_when(enc_type == '0' ~ 144,
                                     lab == 'label'   ~ encsizemap1[enc_size],
                                     lab == 'label_2' ~ encsizemap2[enc_size]),
                      spl = dplyr::case_when(lab == 'label'   ~ encsizestring1[enc_size],
                                      lab == 'label_2' ~ encsizestring2[enc_size]))
  return (x)
}

#' Create Network graph from closures sf layer as input
#'
#' @param x = closures layer in sf format
#' @param directed TRUE if you want a directed graph(default)
#'
#' @export
#' @return a graph of closure layer (no edges)
net_closures <- function(x, directed=TRUE){
  y <- if("name" %in% names(x)) {x} else{net_devices(x)} |>
    dplyr::mutate(node_type='device') |>
      dplyr::distinct(name, .keep_all = TRUE)
  tidygraph::tbl_graph(nodes=y,node_key = "name", directed = directed)
}

#' Create Network graph from premises sf layer as input
#'
#' @param x = Premises layer in sf format
#' @param directed TRUE if you want a directed graph(default)
#'
#' @export
#' @return a graph of premises layer (no edges)
net_uprn <- function(x, directed=TRUE)
{
  graph <- subset(x, !is.na(uprn) & uprn != ' ' ) |>
    dplyr::mutate(node_type='premise', name=as.character(uprn)) |>
      dplyr::distinct(name, .keep_all = TRUE) |>
        tidygraph::tbl_graph(node_key = "name", directed = directed)
  return (graph)
}




#' Create Network graph from cables as inputs (only network cables on the graph)
#'
#' @param x = Cables layer in sf format
#' @param directed TRUE if you want a directed graph(default)
#'
#' @export
#' @return graph Network as tidygraph graph of cables (as edges)
net_cables <- function(x, directed=TRUE){
#  tocheckcables <- dplyr::mutate(subset(cables,!grepl('/',ref)), note='ref not standard')
  x <- x[x$cab_size != '7',] |>
    subset(grepl('./.',ref)) |>
      dplyr::mutate(name=ref)
  cables <- dplyr::mutate(dplyr::rowwise(x),from=strsplit(ref,'/')[[1]][[1]], to=strsplit(ref,'/')[[1]][[2]])
#  tocheckcables <- rbind(tocheckcables,dplyr::mutate(subset(cables,!((from %in% nodes$name) & (to %in% nodes$name))), note='device corrispondence not found'))
  graph <- tidygraph::as_tbl_graph(x= tibble::as_tibble(cables), directed = directed)
  return (graph)
}

#' Create Network graph of virtual cables from splice closures as inputs
#'
#' @param x = splice closures in sf format
#' @param directed TRUE if you want a directed graph(default)
#'
#' @export
#' @return graph Network of only virtual cables (as edges)
net_virtual_cables <- function(x, directed=TRUE){
  y <- dplyr::filter(x, enc_type == 5, !is.na(label),!is.na(label_2))
  t1 <- tibble::tibble(from=y$label, to=y$label_2)

  z <- dplyr::filter(x, enc_type == 6, !is.na(label))
  s <- dplyr::filter(x, enc_type == 7, !is.na(label))
  r <- dplyr::filter(x, enc_type == 7, !is.na(label_2))
  t2 <- tibble::tibble(from=z[s,]$label, to=s[z,]$label)
  t3 <- tibble::tibble(from=z[r,]$label, to=r[z,]$label_2)

  t <- dplyr::bind_rows(t1,t2,t3)
  graph <- tidygraph::as_tbl_graph(t, directed=directed)
  return (graph)
}


#' Boundary validation for graph generation. The function filter the dev boundaries and add R and dev_type values. Return a subset of validated boundaries
#' @param b = boundaries sf
#'
#' @export
#' @return subset of device valid boundaries (sf)
valid_boundaries <- function(b){
  precision <- sf::st_precision(b)
  b <- b[b$level_ %in% c('1','2','3','5','8','9'),]
  b[!sf::st_is_valid(b),] <- sf::st_make_valid(b[!sf::st_is_valid(b),])
 # b <- b[!duplicated(b$name), ]
#  b <- b[!is.na(b$name),]
  b <- dplyr::mutate(b, R=dplyr::case_when(level_==1 ~ 1, level_==2 ~ 8,level_==3 ~ 4, level_==5 ~ 1, level_==8 ~ 32, level_==9 ~ 1),
                        dev_type = dplyr::case_when(level_==1 ~ 'OLT', level_==2 ~ 'ODP1',level_==3 ~ 'ODP2', level_==5 ~ 'ODP3', level_==8 ~ 'JN', level_==9 ~ 'MPT'))
  sf::st_precision(b) <- precision
  return (b)
}

#' Create Network graph from boundaries as inputs (the parent of a boundary is the smallest container)
#'
#' @param b = boundaries layer in sf format
#' @param directed TRUE if you want a directed graph(default)
#'
#' @export
#' @return graph Network as tidygraph graph of boundaries (as nodes)
net_boundaries <- function(b, directed=TRUE){
  #the boundaries must be valid
  b <- valid_boundaries(b)
  lboundaries <- sf::st_within(sf::st_buffer(b,-1),b) #1 meter of negative buffer added to avoid geometries issues
  lboundaries_nodiag <- lapply(seq(lboundaries), \(x) c(lboundaries[[x]][-which(lboundaries[[x]]==x)]))
  boundaries_fathers <- lapply(lboundaries_nodiag, \(x) x[which.min(sf::st_area(sf::st_as_sf(b[x,])))])
  boundaries_connections <- lapply(seq(boundaries_fathers) , \(x) if (length(boundaries_fathers[[x]])>0) c(boundaries_fathers[[x]],x))
  boundaries_connections_df <- data.frame(from=unlist(lapply(boundaries_connections, '[[',1)), to=unlist(lapply(boundaries_connections, '[[',2)))
  graph <- tidygraph::tbl_graph(nodes = b, edges = boundaries_connections_df, directed = directed, node_key = "name")
  return (graph)
}


net_geomgrouped <- function(sfx){
  sfx |> dplyr::group_by(geom) |> dplyr::mutate(geomgroup=dplyr::cur_group_id(), geomdim=dplyr::n()) |> (\(x) lapply(unique(x$geomgroup), \(y) x[x$geomgroup==y,]))()
}

net_geomduplicated <- function(sfx){
  net_geomgrouped(sfx)[sapply(net_geomgrouped(sfx), \(y) if (nrow(y)>1) TRUE else FALSE)]
}

net_groupedbytouch <- function(groupedsfx, sfy){
  groupedsfx |> lapply (\(x) sfy[x,op=sf::st_touches])
}

net_ungroup <- function(gsfx){
  Reduce(\(x,y) rbind(x,y),gsfx)
}

# x = cables, y=premises, z= boundaries, h=closures
net_dropconnect <- function(x,y,z,h){
  bname <- z[y,] |> (\(q) q[!q$level_ %in% c(4,6,7,10,11,12,13,14,15,16),])() |>
    (\(t) t[which.min(sf::st_area(t)),])() |> (\(t) t$name)()
  dnames <- net_devices(h)[x[1,],op=sf::st_touches]$name
  m <- min(nrow(x),nrow(y))
  x[1:m,'to'] <- y[1:m,'uprn']
  if (bname %in% dnames){
    x[1:m,'from'] <- bname
    x[1:m, 'note'] <- ''
  } else {
    x[1:m,'from'] <- ''
    x[1:m, 'note'] <- paste0 ("boundary: ", Reduce(paste,bname), "\ndevices: ", Reduce(paste,dnames))
  }
  return (x)
}

#' Filter the designed premises
#'
#' @param w = white premises (sf)
#' @param g = grey premises (sf)
#' @param cables=cables (sf)
#'
#' @export
#' @return sf of designed premises (all whites + connected greys)
designed_prems <- function(g,cables, w= sf::st_sf(geometry = sf::st_sfc(sf::st_multipoint()))){
  dp <- g[cables[cables['cab_size'==7,]],op=sf::st_touches] |> dplyr::bind_rows(w)
  return (dp)
}

#' Add load to each boundary of a net_boundaries
#'
#' @param bnet = boundaries net (graph)
#' @param dp = designed premises
#' @param cl = splice closures
#'
#' @export
#' @return boundaries net with load on nodes
net_load <- function(bnet, dp){
    dplyr::mutate(bnet, load_acc = unlist(tidygraph::map_bfs_back(tidygraph::node_is_root(), .f = function(node, path, ...) {
      if (nrow(path) == 0) nrow(dp[tidygraph::.N()$geom[node],])
      else {
        nrow(dp[tidygraph::.N()$geom[node],])-sum(unlist(nrow(dp[tidygraph::.N()$geom[path$node[path$parent == node]],]))) +
          sum(c(ceiling(unlist(path$result[path$parent == node])/unlist(tidygraph::.N()$R[path$node[path$parent == node]]))))
      }
    }))) |>
    dplyr::mutate(in_load = ceiling(load_acc / R))
}



#' Create Network graph from cables as inputs (only network cables on the graph)
#' @param x = cables
#' @param y = premises
#' @param w = boundaries
#' @param z = closures
#' @param directed TRUE if you want a directed graph(default)
#'
#' @export
#' @return graph Network as tidygraph graph of drop cables (as edges)
net_drops <- function(x,y,w,z, directed=TRUE){
  gy <- net_geomgrouped(y)
  fx <- x[x$cab_size == '7',]
  fxgy <- net_groupedbytouch(gy, fx)
  drops <- mapply(\(g,h) net_dropconnect(g[1,],h[1,],w,z),fxgy, gy ,SIMPLIFY = FALSE) |> net_ungroup()
  graph <- tidygraph::as_tbl_graph(x= tibble::as_tibble(drops), directed = directed)
  return (graph)
}


#' Create complete Network graph
#' @param cables sf layer
#' @param premises sf layer
#' @param boundaries sf layer
#' @param closures sf layer
#'
#' @export
#' @return graph Network as tidygraph object
net_graph2 <- function(cables,closures,premises, boundaries){
  graph <- tidygraph::graph_join(net_closures(closures), net_uprn(premises), by=c("name","geom","node_type")) |>
    tidygraph::graph_join(net_cables(cables), by=c("name")) |>
      tidygraph::graph_join(net_virtual_cables(closures), by=c("name")) |>
        tidygraph::graph_join(net_drops(cables,premises,boundaries,closures), by=c("name")) |>
          dplyr::mutate(group = tidygraph::group_components(type = "weak"))
  return(graph)
}



# net_cables <- function(cables){
#   sfnetworks::as_sfnetwork(cables) |>
#     tidygraph::activate(edges) |>
#     dplyr::group_by(geom) |>
#     dplyr::mutate(line=dplyr::cur_group_id(), nline=dplyr::n())
# }


#' Create simple Network plot
#'
#' @param net graph of tidygraph class
#' @param closures layout as from ggraph library
#'
#' @export
#' @return graph Network preliminary plot
net_plot <- function(net,...){
  g <- ggraph::ggraph(net, ...)+
    ggraph::geom_edge_fan(ggplot2::aes(width=0.1))+
    ggiraph::geom_point_interactive(ggplot2::aes(x=x,y=y, tooltip=name, data_id=group), size=1, stroke=0.1)+
    ggraph::theme_graph(base_family="Roboto")
  ggiraph::girafe(ggobj = g,
                  options = list(
                    ggiraph::opts_tooltip(use_fill = TRUE),
                    ggiraph::opts_sizing(rescale = TRUE,width = 0.9),
                    ggiraph::opts_zoom(max = 15)))
}



#' Create simple Network plot for boundaries
#'
#' @param x graph of tidygraph class (the nodes must contain the label_text field with the string for plot)
#' @param name the name of the file output (the output is an svg file, so a name ending as '.svg' is better)
#' @param directory the place where to place the plot
#' @param layout type of the graph
#'
#' @export
#' @return graph Network preliminary plot
net_bplot <- function(x, name='temp.svg',directory=paste0(getwd(),'/boundaries'),layout='tree'){
  layout <- ggraph::create_layout(x, layout)
  layout$y <- -layout$y
  yr <- max(sapply(unique(layout$x), (\(k) length(layout$y[layout$x==k]))))
  xr <- max(sapply(unique(layout$y), (\(k) length(layout$x[layout$y==k]))))
  p <- ggraph::ggraph(layout) +
    ggraph::geom_edge_diagonal(aes(colour=warn),width = 0.5,alpha=0.5) +
    ggraph::geom_node_label(aes(label = label_text,colour = as.factor(level_), fill=alert), repel=FALSE)+
    ggplot2::scale_colour_manual(values=c("2"="#126b10", "3"="#b81707", "5"="orange","8"="pink","9"="green","1"="black", "TRUE"="red", "FALSE"="blue"))+
    ggplot2::scale_fill_manual(values=c("TRUE"="#fce15d", "FALSE"="white"))+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(limits = c(min(layout$y)-1, max(layout$y)+1))+
    ggplot2::scale_x_continuous(limits = c(min(layout$x)-3, max(layout$x)+3))+
    ggplot2::theme(legend.position="none")
  dir.create(directory, showWarnings = FALSE)
  ggplot2::ggsave(plot=p,filename=file.path(directory, name),device = 'svg',width = 14*yr,height=0.85*xr+2,units="in", limitsize=FALSE)
  browseURL(file.path(directory, name))
}


#' Create SLD schema
#'
#' @param cables Cables layer in sf format
#' @param premises Premises layer in sf format
#' @param closures Closures layer in sf format
#' @param boundaries Boundaries layer in sf format
#' @param out_param Short prefix for outputs
#' @param logo PNG for output logo
#'
#' @export
#' @return graph Network as tidygraph graph
sld <- function(cables,premises,closures,boundaries, out_param, logo='C:/Users/DomenicoGuadalupi/OneDrive - Viberoptix/Documents/R/SLD/viberoptix_logo_testo.png')
{
  arcgisbinding::arc.check_product()

  sysfonts::font_add_google("Roboto", "Roboto")

  prefix=out_param
  dir.create(prefix)

  showtext::showtext_auto()

  graph_dim <-function(graph){
    graph_dim_root<-function(graph,rootid){
      ###
      n <- tidygraph::activate(graph, nodes)
      n <- dplyr::mutate(n, bfs_results = tidygraph::bfs_dist(root = rootid, mode='out'))
      n <- tibble::as_tibble(n)
      n <- dplyr::group_by(n,bfs_results)
      n <- dplyr::summarise(n())
      n <- n[!is.na(n$bfs_results),]
      n <- sapply(n,(function(x)max(x,na.rm = TRUE)))
      n <- (function(n)(c(n[[1]],n[[2]])))
      return(n)
      ###
    }

    purrr::map(roots_id(graph), (function (x) graph_dim_root(graph,x)))}



  plot_subgraphs <- function(graph, plot_name="network", dim=c(50,100), ratio=2, legend='top'){

    plot_graph <- function (g, ratio){
      ggraph::ggraph(g) +
        ggraph::geom_edge_diagonal(width = 0.2,alpha=0.2) +
        ggraph::geom_node_label(ggplot2::aes(label = label_text,colour = as.factor(enc_types[as.integer(enc_type)+1])), size=3, label.padding=unit(0.25,"lines"), repel=FALSE)+
        ggraph::geom_node_text(ggplot2::aes(label = node_text), size=3, hjust=0.5, vjust=-0.2, repel=FALSE) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_reverse() +
        ggplot2::labs(title=paste(prefix," - SINGLE LINE DIAGRAM<br>",plot_name,paste0("<br><img src='",logo, "', width='350' /><br>")))+
        ggraph::theme_graph(base_family="Roboto")+
        ggplot2::theme(aspect.ratio = ratio,
              legend.position=legend,
              legend.text=ggplot2::element_text(size=11),
              plot.title.position = "plot",
              #plot.title = ggplot2::element_textbox_simple(),
              plot.margin=grid::unit(c(0,0,0,0), "mm"))+
        ggplot2::guides(colour = ggplot2::guide_legend(title="Enclosure Type:"))
    }


    subgraph_connected <- tidygraph::activate(graph, nodes)
    subgraph_connected <- dplyr::mutate(subgraph_connected,group = as.factor(tidygraph::group_components(type = "weak")))

    #count # of groups
    m <- dplyr::count(unique(as.list(subgraph_connected)$nodes['group']))$n
    deepness <- dplyr::mutate(subgraph_connected, deep = tidygraph::node_distance_from(tidygraph::node_is_root()))
    deepness <- tidygraph::activate(deepness, nodes)
    deepness <- tibble::as_tibble(deepness)
    deepness <- max(deepness$deep) +1



    for (i in seq(1,m)){
      sub_i <- dplyr::filter(subgraph_connected, group==i)
      nodes_n <- dplyr::count(as.list(sub_i)$nodes)$n
      if (nodes_n > 2){
        gdim <- unlist(graph_dim(sub_i))
        plt <- plot_graph(sub_i, ratio=1)
        yr=diff(ggplot2::ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range)
        xr=diff(ggplot2::ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range)
        xmin=ggplot2::ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range[1]
        xmax=ggplot2::ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range[2]
        ymin=ggplot2::ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range[1]
        ymax=ggplot2::ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range[2]
        r=(yr)/(xr+0.5)
        plt <- plot_graph(sub_i, ratio=1)+expand_limits(y = c(-0.5,deepness+0.5), x=c(-0.5+xmin,xmax+0.5))
        fname=paste0(gsub("[[:punct:][:blank:]]",'', plo_tname),'.svg')
        ggplot2::ggsave(plot=plt, path=prefix,filename=fname,device = 'svg',width = 5*(deepness+1)^1,height=5*(yr+1)^1,units="in", limitsize=FALSE)#
      }
    }
  }

  plot_subgraphs2 <- function(graph, plot_name="network", dim=c(50,100), ratio=2, legend='left'){

    plot_graph <- function (g){
      ggraph::ggraph(g) +
        ggraph::geom_edge_diagonal(width = 0.2,alpha=0.2) +
        ggraph::geom_node_label(ggplot2::aes(label = label_text,colour = as.factor(enc_types[as.integer(enc_type)+1])), size=3, label.padding=grid::unit(0.25,"lines"), repel=FALSE)+
        ggraph::geom_node_text(ggplot2::aes(label = node_text), size=3, hjust=0.5, vjust=-0.2, repel=FALSE) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_reverse() +
        ggplot2::labs(title=paste(prefix, " - SINGLE LINE DIAGRAM2<br>",plot_name,paste0("<br><img src='",logo, "', width='350' /><br>")))+
        ggraph::theme_graph(base_family="Roboto")+
        ggplot2::theme(aspect.ratio = ratio,
              legend.position=legend,
              legend.text=ggplot2::element_text(size=11),
              plot.title.position = "plot",
              #plot.title = ggplot2::element_textbox_simple(),
              plot.margin=grid::unit(c(0,0,0,0), "mm"))+

        ggplot2::guides(colour = ggplot2::guide_legend(title="Enclosure Type:"))
    }




    subgraph_connected <- dplyr::mutate(tidygraph::activate(graph,nodes),group = as.factor(tidygraph::group_components(type = "weak")))

    #count # of groups
    m=dplyr::count(unique(as.list(subgraph_connected)$nodes['group']))$n


    for (i in seq(1,m)){
      sub_i <- dplyr::filter(subgraph_connected, group==i)
      if (dplyr::count(as.list(sub_i)$nodes)$n > 1){
        plot_graph(sub_i)+ggplot2::expand_limits(y = c(-0.5,2.5))
        fname=paste0(gsub("[[:punct:][:blank:]]",'',plot_name),'_',i,'.svg')
        ggplot2::ggsave(path=prefix,filename=fname,device = 'svg',width = dim[1],height=dim[2], limitsize=FALSE)
      }
    }
  }

  closures <- dplyr::mutate(closures, status=as.integer(status),owner=as.integer(owner), placement=as.integer(placement), node_type='device')
  closures2 <- dplyr::filter(closures,label_2 != " ")
  closures2 <- dplyr::mutate(closures2,label=label_2)
  cables_nodrop <- dplyr::filter(cables, cab_size!='7')
  cables_nodrop  <-  dplyr::mutate(cables_nodrop, status=as.integer(status),owner=as.integer(owner), placement=as.integer(placement), node_type='cable')

  nodes <- dplyr::bind_rows(cables_nodrop,closures, closures2)

  # nodes %>% mutate(name=ifelse(!is.na(ref) & ref!=" ", ref, label)) -> nodes
  nodes <- dplyr::mutate(nodes,name=ifelse(node_type=="cable", ref, label))

  # nodes %>% filter(!is.na(ref) & ref!=" ") %>% .$ref -> from1
  temp <- dplyr::filter(nodes, node_type=="cable")
  refs <- temp$ref
  from <- stringr::str_extract(refs, "(^.+?)/")
  from <- stringr::str_remove(from, '/')
  from <- c(refs,from)

  temp <-  stringr::str_extract(refs,"/(.+?$)")
  to <- stringr::str_remove(temp, '/')
  to<-c(to,refs)

  edges<-tibble::tibble(from=from, to=to)

  cable_sizes<-c('12F','24F','48F','96F','144F','288F','1F','4F','8F','60F')
  enc_types<-c('OLT','ODP1','ODP2','ODP3','Joint','ODP1/ODP2','Junction Node','MPT','MDU')
  sp_types<-c('1:8','1:4','None','1:32')
  placements<-c('UG','OH','UG/OH')

  splitters<-function(sp1_type, sp2_type, sp3_type){
    sp<-c(sp1_type, sp2_type, sp3_type)
    sp[is.na(sp)]<-3
    nxsp=rep("",4)
    for (i in c(1,2,4)) {
      nxsp[i]=ifelse(sum(sp==i)>0, paste0('+',sum(sp==i),'x', sp_types[i]), "")}
    return (paste0('\n',substring(paste(nxsp,collapse=''),2)))
  }

  nodes <- dplyr::mutate(nodes, node_text=
                     ifelse((!is.na(ref) & ref!=" "),
                            paste0(ref,"\n",
                                   cable_sizes[as.integer(cab_size)]," / ",
                                   as.integer(sf::st_length(sf::st_geometry(nodes))),"m",
                                   "(",placements[as.integer(placement)],")"),
                            ""))

  nodes <-  dplyr::mutate(nodes,spl=NULL, label_text=NULL)
  nodes <- dplyr::mutate(dplyr::rowwise(nodes),spl=splitters(sp1_type,sp2_type,sp3_type))
  nodes$spl[nodes$spl=="\n"]=""

  ##################
  bounds <- tibble::as_tibble(boundaries)[,c('name','t_homes')]
  nodes <- dplyr::left_join(nodes,bounds)
  ################


    nodes <- dplyr::mutate(dplyr::rowwise(nodes),label_text=ifelse((!is.na(label) & label!=" "),
                     paste('HC: ',t_homes,'\n',
                           label,spl,'\n',
                           'X:',round(sf::st_coordinates(geom)[1],digits=1),
                           ' Y:',round(sf::st_coordinates(geom)[2],digits=1)),
                     NA))


  graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, node_key = "name", directed = TRUE)

  roots <-function(graph){temp <- dplyr::filter(graph, tidygraph::node_is_root())
                          temp <-  tibble::as_tibble(temp)
                          temp$name}

  #Extract id of a node from its name
  node_id <-function(graph,node_name){temp <- tidygraph::activate(graph,nodes)
                                            temp <- tibble::as_tibble(temp)
                                            which(temp$name==node_name)}

  #Extract id of roots from a graph
  roots_id <-function(graph){purrr::map(roots(graph), (function(x) node_id(graph,x)))}

  #Create the tree starting on a node
  sub_tree <-function(graph, root_id){
      temp <- tidygraph::activate(graph,nodes)
      temp <- dplyr::mutate(temp, bfs_results = tidygraph::bfs_dist(root = root_id, mode='out'))
      dplyr::filter(temp,!is.na(bfs_results))
  }

  #Create all trees starting on roots of a graph
  sub_trees<-function(graph){purrr::map(roots_id(graph), (function (x) sub_tree(graph,x)))}

  purrr::map(sub_trees(graph),(function(x) plot_subgraphs(x, plot_name=(paste(prefix,'- Network ',roots(x))))))

  graph_JN<-function(graph){
      temp <- tidygraph::activate(graph,nodes)
      temp <- dplyr::mutate(temp, filter1 = !enc_type %in% c(7,8))
      temp <- dplyr::filter(temp, filter1)
      temp <- tidygraph::as_tbl_graph(temp)
      temp <- dplyr::mutate(temp, filter2 = !(tidygraph::node_is_leaf() & is.na(enc_type)))
      dplyr::filter(temp, filter2)}

  purrr::map(
    purrr::map(sub_trees(graph),graph_JN),
    function(x)(
      plot_subgraphs2(x,paste0(prefix,' network JN - ',roots(x)), dim=c(33,23),ratio=0.5, legend='top')
    ))

  #####################################################
  #####################################################
  #####################################################
  #####################################################


  sub_tree_deep <-function(graph, root_id, deep=3){
      temp <- tidygraph::activate(graph,nodes)
      temp <- dplyr::mutate(temp, bfs_results = tidygraph::bfs_dist(root = root_id, mode='out'))
      temp <- dplyr::filter(temp,!is.na(bfs_results))
      dplyr::filter(temp, !is.na(bfs_results) & bfs_results<deep)}

  graph_nodes<- function(x){temp <- tidygraph::activate(x,nodes)
                            tibble::as_tibble(temp)$name}

  sub_trees_deep<- function(graph,deep=3){
    purrr::map(
      purrr::map(graph_nodes(dplyr::filter(graph_JN(graph), !is.na(enc_type) & enc_type!=" ")),
                 (function(x) node_id(graph,x))),
               function(x) sub_tree_deep(graph,x,deep=deep)
    )}

  purrr::map(sub_trees_deep(graph, deep=3),
      function(x)(
        plot_subgraphs2(x,paste0(prefix,' JN - ',roots(x)), dim=c(11,16),ratio=1.4, legend='top')))

  return (graph)
}

