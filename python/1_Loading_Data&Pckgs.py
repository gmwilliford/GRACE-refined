import os
import numpy as np
import rasterio
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.mask import mask
from rasterio.transform import from_origin
import geopandas as gpd
from netCDF4 import Dataset

# Set working directory
os.chdir("/Users/brian/Projects/GRACE-refined/GRACE_Comp/")

# List to hold clipped GRACE files
grace_clipped_list = []
grace_files = [f for f in os.listdir(".") if f.startswith("GRD") and f.endswith(".nc")]

# Loop through GRACE files and process each
for grace_file in grace_files:
    # Open
    grace_nc = Dataset(grace_file)
    grace_var = "lwe_thickness"
    grace_data = grace_nc.variables[grace_var][:]
    grace_mtrx = np.matrix(grace_data)
    src = rasterio.open(grace_file)
    # Read raster
    """ with rasterio.open(grace_file) as src:
        #transform = from_origin(lon_bounds[0], lat_bounds[1], ds.lon.diff('lon').mean().item(), ds.lat.diff('lat').mean().item())
        if src.count == 0:
            print(f"No bands found in {grace_file}")
            continue
        grace_rst = src.read(1)
        transform = src.transform
    
    # Shift extent of GRACE raster by 180 degrees (necessary for proper alignment)
    """
    crs = 4326 # WGS 84
    transform, width, height = calculate_default_transform(
        crs, crs, src.width, src.height, *src.bounds, dst_width=src.width, dst_height=src.height
    )
    kwargs = src.meta.copy()
    kwargs.update({
        'crs': src.crs,
        'transform': transform,
        'width': width,
        'height': height
    })
    
    """ with rasterio.open('rotated.tif', 'w', **kwargs) as dst:
        reproject(
            source=rasterio.band(src, 1),
            destination=rasterio.band(dst, 1),
            src_transform=src.transform,
            src_crs=src.crs,
            dst_transform=transform,
            dst_crs=src.crs,
            resampling=Resampling.nearest
        ) """
    
    # MS Embayment Shapefile
    shapefile = gpd.read_file("/Users/brian/Projects/GRACE-refined/MS_Embayment_SHP/Mississippi_embayment.shp")
    shapefile_proj = shapefile.to_crs(crs)
    
    # Cropping GRACE by shapefile
    """ with rasterio.open('rotated.tif') as src:
        out_image, out_transform = mask(src, shapefile_proj.geometry, crop=True)
        out_meta = src.meta.copy()
        out_meta.update({"driver": "GTiff",
                         "height": out_image.shape[1],
                         "width": out_image.shape[2],
                         "transform": out_transform}) """
    out_image, out_transform = mask(src, shapefile_proj.geometry, crop=True)
    out_meta = src.meta.copy()
    with rasterio.open("clipped.tif", "w", **out_meta) as dest:
        dest.write(out_image, 1)
    
    # Append to list
    grace_clipped_list.append(out_image)
    grace_nc.close()

# MONTHLY GLDAS 1 degree by 1 degree resolution
folder_path = "/Users/brian/Projects/GRACE-refined/GLDAS_1x1/"
var_names = ["Evap_tavg", "Qs_acc", "SWE_inst", "CanopInt_inst", "RootMoist_inst", "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst", "Rainf_f_tavg"]

gldas1_stacked = []

# Loop over each yearly .nc4 file
for file_name in [f for f in os.listdir(folder_path) if f.endswith(".nc4")]:
    var_rasters = {}
    
    # Loop over variable names and extract each one as a raster
    with Dataset(os.path.join(folder_path, file_name)) as ds:
        for var_name in var_names:
            var_rasters[var_name] = ds.variables[var_name][:]
    
    cur_gldas1_clipped = {}
    for var_name in var_names:
        cur_raster = var_rasters[var_name]
        with rasterio.open(os.path.join(folder_path, file_name)) as src:
            out_image, out_transform = mask(src, shapefile_proj.geometry, crop=True)
            cur_gldas1_clipped[var_name] = out_image
    
    gldas1_stacked.append(cur_gldas1_clipped)

# MONTHLY GLDAS at 0.25x0.25 resolution
folder_path025 = "/Users/brian/Projects/GRACE-refined/GLDAS_025x025/"
gldas025_stacked = []

# Loop over each yearly .nc4 file
for file_name in [f for f in os.listdir(folder_path025) if f.endswith(".nc4")]:
    var_rasters = {}
    
    # Loop over variable names and extract each one as a raster
    with Dataset(os.path.join(folder_path025, file_name)) as ds:
        for var_name in var_names:
            var_rasters[var_name] = ds.variables[var_name][:]
    
    cur_gldas025_clipped = {}
    for var_name in var_names:
        cur_raster = var_rasters[var_name]
        with rasterio.open(os.path.join(folder_path025, file_name)) as src:
            out_image, out_transform = mask(src, shapefile_proj.geometry, crop=True)
            cur_gldas025_clipped[var_name] = out_image
    
    gldas025_stacked.append(cur_gldas025_clipped)

print(gldas025_stacked)