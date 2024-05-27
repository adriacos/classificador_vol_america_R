source("./classificador_vol_america/scripts/clump_vector.R")
print("clump_vector_combine.do_big.files_launch_km")

load("./classificador_vol_america/temp/current_km.RData")
load("./classificador_vol_america/temp/clump_vector_bigarea.RData")
print(paste(current_km,"km",sep=""))
unlink("./classificador_vol_america/vect/clumped/temp/")


load("./classificador_vol_america/temp/oris_km.RData")
load("./classificador_vol_america/temp/oris_done.RData")
oris_km <- oris_km$get_by_km(ikm)
oris <- unique(sapply(oris_km,function(o){o$ori}))
p.n.cores <- min(sapply(oris_km,function(o){o$pncores}))
vects_quad_ori_list <- list()
save(vects_quad_ori_list,file="./classificador_vol_america/temp/vects_quad_ori_list.RData")

lapply(oris[!oris%in%oris_done],function(ori,current_km){
        current_ori <- ori
        save(current_ori,file="./classificador_vol_america/temp/current_ori.RData")
        vects_quad_ori_list <- list()
        save(vects_quad_ori_list,file="./classificador_vol_america/temp/vects_quad_ori_list.RData")
        restartSession(command=source("./classificador_vol_america/scripts/launch_clump_vector_big_files.R"))
        #launch_clump_vector_big_files
})

unlink("./classificador_vol_america/temp/clump_vector_bigarea.RData")


oris_done <- c()
save(oris_done, file="./classificador_vol_america/temp/oris_done.RData")

kms_done <- append(kms_done,current_km)
save(kms_done, file="./classificador_vol_america/temp/kms_done.RData")

restartSession(command=source("./classificador_vol_america/scripts/clump_vector_combine.do_bigfiles.R"))