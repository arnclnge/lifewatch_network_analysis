####INTERPOLATION MAPS: Run inside QGIS Python console

import pandas as pd

## SET-UP DIRECTORIES
out_dir           = 'C:/Users/arienne.calonge/Workspace/REI_interpolation/'

## SET-UP INPUTS

REI_map = out_dir+'REI_map.csv'
    
#Read csv
REI_csv = pd.read_csv(REI_map)
REI_cols = list(REI_csv.columns)[1:6]

Processing algorithms

for i in REI_cols: 
    shp_path = out_dir+i+'.shp'
    processing.run("native:createpointslayerfromtable",{ 'INPUT' : REI_map, 'MFIELD' : '', 'OUTPUT' : shp_path, 'TARGET_CRS' : QgsCoordinateReferenceSystem('EPSG:4326'), 'XFIELD' : 'deploy_longitude', 'YFIELD' : 'deploy_latitude', 'ZFIELD' : i })
    
    #interpolate from point to raster
    processing.run("qgis:idwinterpolation", { 'DISTANCE_COEFFICIENT' : 2, 'EXTENT' : '2.186337008,4.304755306,51.069644339,51.895784447 [OGC:CRS84]', 'INTERPOLATION_DATA' : shp_path+'::~::1::~::-1::~::0', 'OUTPUT' : out_dir+i+'.tif', 'PIXEL_SIZE' : 0.001377 })
    
    #add output shp and raster
    REI_layer=QgsVectorLayer(shp_path,i)
    interp = QgsRasterLayer(out_dir+i+'.tif', i+'.tif') 
    
    QgsProject.instance().addMapLayer(interp)
    QgsProject.instance().addMapLayer(REI_layer)
    
#add animal release locations
an_list = ["Alosa fallax", "Anguilla anguilla", "Gadus morhua", "Dicentrarchus labrax"]

for i in an_list:
    #add animal release csv
    an_release = out_dir+'csv/'+i+'_release.csv'

    release_shp_path = out_dir+i+'_release'+'.shp'
    processing.run("native:createpointslayerfromtable",{ 'INPUT' : an_release, 'MFIELD' : '', 'OUTPUT' : release_shp_path, 'TARGET_CRS' : QgsCoordinateReferenceSystem('EPSG:4326'), 'XFIELD' : 'release_longitude', 'YFIELD' : 'release_latitude', 'ZFIELD' : 'scientific_name'})
    
    #add release shp as a layer
    release_layer=QgsVectorLayer(release_shp_path,i)
    QgsProject.instance().addMapLayer(release_layer)
    
#add boundary shpfiles from marineregions.org
belg_eez = main_dir+ 'shp/belgium_eez/eez.shp'
schelde = main_dir+ 'shp/Westerschelde/seavox_v18.shp'

belg_eez_lyr = QgsVectorLayer(belg_eez, "eez", "ogr")
schelde_lyr = QgsVectorLayer(schelde, "schelde", "ogr")

if not belg_eez_lyr.isValid():
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(belg_eez_lyr)
    QgsProject.instance().addMapLayer(schelde_lyr)
