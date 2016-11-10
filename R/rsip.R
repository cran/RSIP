# Rsip
#
#

maskFile <- function(shp_poligon, nam="RSIP_", dimname=c(17,26))
    #--------------------------------------------------------------------------
    # AUTOR         : Iván Arturo Ayala Bizarro
    # PARÁMETROS    :
    # PROPÓSITO     : Make mask from limit catchment
    #--------------------------------------------------------------------------
{
    #### Cargar límite de la cuenca o area de estudio
    cuenca<-readOGR('.',shp_poligon)
    cuenca<- spTransform(cuenca, CRS('+proj=longlat +datum=WGS84'))
    #spplot(cuenca, "NOMBRE")

    #### Cargar el Raster para el tratamiento de datos
    files<- list.files(pattern='.tif') # Extraer listado de archivos

    ## Cargar imagen toda la imagen
    imageraster <- raster(files[1])

    ## Cortar para la cuenca
    imageraster <- crop(imageraster, cuenca)

    ## Hacer mascara para cuenca
    imageraster <- mask(imageraster, cuenca)

    ## Extraer nombre del archivo
    names(imageraster) <- substr(files[1], dimname[1], dimname[2])
    names(imageraster) <- paste0(nam,names(imageraster))
    cat(sprintf('\n RSIP: Processing raster %s \n',names(imageraster)))

    # Guardar en formato Tif
    writeRaster(imageraster,names(imageraster), format = "GTiff",overwrite = T)

    if(length(files)>=2){
        for (i in 2:length(files))
        {
            imageraster <- raster(files[i])             # cargar imagen
            imageraster <- crop(imageraster, cuenca)   #cortar para la cuenca
            imageraster <- mask(imageraster, cuenca)   # hacer mascara para la cuenca
            names(imageraster) <- substr(files[i], dimname[1], dimname[2]) #extraer nombre del archivo
            names(imageraster) <- paste0(nam,names(imageraster))
            writeRaster(imageraster, names(imageraster), format = "GTiff",overwrite = T) # Guardar en formato Tif
            cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))
        }
    }
}


exportValueGrid <- function()
    #--------------------------------------------------------------------------
    # AUTOR         : Iván Arturo Ayala Bizarro
    # PARÁMETROS    :
    # PROPÓSITO     : Plotea todos los datos con limite de cuenca
    #--------------------------------------------------------------------------
{
    #### Cargar el Raster para el tratamiento de datos
    files<- list.files(pattern='.tif') # Extraer listado de archivos

    ## Cargar imagen toda la imagen
    imageraster <- raster(files[1])

    ## Extraer nombre del archivo

    cat(sprintf('\n RSIP: Processing raster %s \n',names(imageraster)))
    val<-rasterToPoints(imageraster)

    #vals<-extract(imageraster,cuenca) # extract(imageraster, cuenca)
    #coord<-cellFromPolygon(imageraster,cuenca) # Tomar las coordenadas
    #combine<-cbind(coord[[1]],vals[[1]]) # Combinar
    combine<-cbind(val[,1],val[,2],val[,3]) # Combinar

    if(length(files)>=2){
        for (i in 2:length(files))
        {
            imageraster <- raster(files[i])             # cargar imagen

            # Imprimir resultados
            val<-rasterToPoints(imageraster)
            combine<-cbind(combine,val[,3])
            cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))
        }
    }

    write.table(combine,paste0('RSIP_ValueGrid','.txt'))
    cat('\nRSIP: Successful Analysis! - The Values were printed correctly \n')
}


exportValuePoligon <- function(shp_poligon)
    #--------------------------------------------------------------------------
    # AUTOR         : Iván Arturo Ayala Bizarro
    # PARÁMETROS    :
    # PROPÓSITO     : Extrae los valores raster desde poligono shape
    #                 como archivos .shp
    #--------------------------------------------------------------------------
{
    # Leer el límite de la cuenca
    cuenca <- readOGR('.',shp_poligon)

    # Cargar el Raster
    files<- list.files(pattern='.tif') # Extraer listado de archivos
    imageraster <- raster(files[1])   #cargar imagen

    cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))
    rbrick <- imageraster ## Asignar a la variable rbrick

    if(length(files)>=2){
        for (i in 2:length(files))
        {
            imageraster <- raster(files[i]) #cargar imagen
            rbrick <- addLayer(rbrick,imageraster) # acumular las imágenes en una misma variable raster
            cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))
        }
    }
    ## Extraer Valores de Cuenca
    cat('\n RSIP: Extracting basin values...\n')
    Pm_c <-extract(rbrick, cuenca[1:1, ])
    write.table(Pm_c, file = paste0('RSIP_ValuePoligono','.csv'),row.names=T, na="",col.names=T, sep="\t")
    cat('\nRSIP: Successful Analysis! - The Values were printed correctly \n')
}


exportValuePointShp <- function(shp_station)
    #--------------------------------------------------------------------------
    # AUTOR         : Iván Arturo Ayala Bizarro
    # PARÁMETROS    :
    # PROPÓSITO     : Extrae los valores raster desde estaciones consideradas
    #                 como archivos .shp (campo point)
    #--------------------------------------------------------------------------
{

    ## Cargar las estaciones desde archivo shapefile
    estaciones <- readOGR('.',shp_station)



    # Reproyectar las estaciones a lat/lon WGS84 ("geographic projection")
    estaciones.ll <- spTransform(estaciones, CRS("+proj=longlat"))

    # Cargar el Raster
    files<- list.files(pattern='.tif') # Extraer listado de archivos
    imageraster <- raster(files[1])   #cargar imagen

    cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))
    rbrick <- imageraster ## Asignar a la variable rbrick

    if(length(files)>=2){
        for (i in 2:length(files))
        {
            imageraster <- raster(files[i]) #cargar imagen
            rbrick <- addLayer(rbrick,imageraster) # acumular las imágenes en una misma variable raster
            cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))
        }
    }
    cat('\n RSIP: Extracting Values from Stations...\n')
    Pm_e <-extract(rbrick,estaciones)
    name_station<-estaciones@data[[1]]
    row.names(Pm_e)<- name_station

    write.table(Pm_e,paste0('RSIP_ValuePointShp','.txt'))
    cat('\nRSIP: Successful Analysis! - The Values were printed correctly \n')

}

exportValuePointsTxt<- function(txt_xy)
    #--------------------------------------------------------------------------
    # AUTOR         : Iván Arturo Ayala Bizarro
    # PARÁMETROS    :
    # PROPÓSITO     : Extrae los valores raster desde estaciones consideradas
    #                 en archivos .txt (lon lat name)
    #--------------------------------------------------------------------------
{
    xy<-cbind(txt_xy[,1],txt_xy[,2])
    # Cargar el Raster
    files<- list.files(pattern='.tif') # Extraer listado de archivos
    imageraster <- raster(files[1])   #cargar imagen
    cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))
    rbrick <- imageraster ## Asignar a la variable rbrick

    if(length(files)>=2){
        for (i in 2:length(files))
        {
            imageraster <- raster(files[i]) #cargar imagen
            rbrick <- addLayer(rbrick,imageraster) # acumular las imágenes en una misma variable raster
            cat(sprintf('Processing raster %s \n',names(imageraster)))
        }
    }
    cat('\n RSIP: Extracting Values from File...\n')
    Pm_e <-extract(rbrick,xy)
    #row.names(Pm_e)<- txt_xy[,3]

    df1 <- data.frame(Pe = Pm_e)
    rownames(df1) <- txt_xy[,3]
    write.table(df1,paste0('RSIP_ValuePointsTxt','.txt'))
    print(df1)
    cat('\nRSIP: Successful Analysis! - The Values were printed correctly \n')
}



plotAll <- function(dimplot = c(3,4), color = c("red", "yellow",'green3',"cyan", "blue"),
                    zlim=c(-10,2000), dimname=c(17,23))
    #--------------------------------------------------------------------------
    # AUTOR         : Iván Arturo Ayala Bizarro
    # PARÁMETROS    :
    # PROPÓSITO     : Plotea todos los datos sin cortar los raster
    #--------------------------------------------------------------------------
{
    #### Cargar los Rasters para el tratamiento de datos
    files<- list.files(pattern='.tif') # Extraer listado de archivos

    ## Cargar imagen toda la imagen
    imageraster <- raster(files[1])

    ## Extraer nombre del archivo
    names(imageraster) <- substr(files[1], dimname[1], dimname[2])
    cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))

    ## Asignar a la variable rbrick
    rbrick <- imageraster

    if(length(files)>=2){
        for (i in 2:length(files))
        {
            imageraster <- raster(files[i])             # cargar imagen
            names(imageraster) <- substr(files[i], dimname[1], dimname[2]) #extraer nombre del archivo
            rbrick <- addLayer(rbrick,imageraster)  # acumular en una misma variable raster
            cat(sprintf('RSIP: Processing raster %s \n',names(imageraster)))
        }
    }

    #### Ploteo de los raster tiff
    spplot(rbrick,layout=dimplot, zlim = zlim, col.regions=colorRampPalette(color)(255))
    cat('\nRSIP: Successful Analysis! - The imagen were printed correctly \n')
}



trmmToTiff <- function()
    #--------------------------------------------------------------------------
    # AUTOR         : Iván Arturo Ayala Bizarro
    # PARÁMETROS    :
    # PROPÓSITO     : Transforma el archivo .nc4 del producto TRMM
    #               (ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42_Daily.7/)
    #                en formatos .tif (raster) con valores de precipitaciones diarias
    #--------------------------------------------------------------------------
{
    files<- list.files(pattern='.nc4') # Extraer listado de archivos

    for (i in 1:length(files))
    {
        #dat.ncdf4 <- nc_open('3B42_Daily.20160229.7.nc4')
        dat.ncdf4 <- nc_open(files[i])
        lon <- ncvar_get(dat.ncdf4, 'lon')
        lat <- ncvar_get(dat.ncdf4, 'lat')
        Pdiario <- ncvar_get(dat.ncdf4, 'precipitation')
        #head(lon)

        d1 <- expand.grid(x = lon, y = lat)
        p<-as.vector(t(Pdiario))

        df <- data.frame( x = d1$x, y = d1$y, P_daily = p)
        dfr<-rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value
        projection(dfr) <- CRS("+proj=longlat +datum=WGS84")
        #plot(dfr)
        writeRaster(dfr, filename = dat.ncdf4$filename, format = 'GTiff', overwrite = T)
        cat(sprintf('RSIP: Processesing raster %s \n',dat.ncdf4$filename))
    }


}


