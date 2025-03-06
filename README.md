# GRACE-refined
The final, refined scripts from the GRACE USGS EDMAP project, U of M dept. of Earth Sciences.

These scripts are intended to be run in a sequence, as indicated by the numbering on each file.  


# GLDAS and NLDAS reference

# GLDAS vs. NLDAS Field Comparison

## GLDAS Fields

| Field Name       | Description                            | Units          |
|------------------|----------------------------------------|----------------|
| Swnet            | Net shortwave radiation flux           | W m⁻²          |
| Lwnet            | Net longwave radiation flux            | W m⁻²          |
| Qle              | Latent heat net flux                   | W m⁻²          |
| Qh               | Sensible heat net flux                 | W m⁻²          |
| Qg               | Ground heat flux                       | W m⁻²          |
| Snowf            | Snow precipitation rate                | kg m⁻² s⁻¹     |
| Rainf            | Rain precipitation rate                | kg m⁻² s⁻¹     |
| Evap             | Evapotranspiration                     | kg m⁻² s⁻¹     |
| Qs               | Storm surface runoff                   | kg m⁻²         |
| Qsb              | Baseflow-groundwater runoff            | kg m⁻²         |
| Qsm              | Snow melt                              | kg m⁻²         |
| AvgSurfT         | Average surface skin temperature       | K              |
| Albedo           | Albedo                                 | %              |
| SWE              | Snow depth water equivalent            | kg m⁻²         |
| SnowDepth        | Snow depth                             | m              |
| SoilMoist        | Soil moisture                          | kg m⁻²         |
| SoilTMP          | Soil temperature                       | K              |
| PotEvap          | Potential evaporation rate             | W m⁻²          |
| ECanop           | Canopy water evaporation               | W m⁻²          |
| Tveg             | Transpiration                          | W m⁻²          |
| ESoil            | Direct evaporation from bare soil      | W m⁻²          |
| RootMoist        | Root zone soil moisture                | kg m⁻²         |
| CanopInt         | Plant canopy surface water             | kg m⁻²         |
| SnowT            | Snow surface temperature               | K              |
| Acond            | Aerodynamic conductance                | m s⁻¹          |
| TWS              | Terrestrial water storage              | mm             |
| GWS              | Groundwater storage                    | mm             |
| Wind_f           | Wind speed                             | m s⁻¹          |
| Rainf_f          | Total precipitation rate               | kg m⁻² s⁻¹     |
| Tair_f           | Temperature                            | K              |
| Qair_f           | Specific humidity                      | kg/kg          |
| Psurf_f          | Pressure                               | Pa             |
| SWdown_f         | Downward shortwave radiation flux      | W m⁻²          |
| LWdown_f         | Downward longwave radiation flux       | W m⁻²          |

*Note: The suffixes `_f`, `_tavg`, `_acc`, and `_inst` indicate forcing variables, time-averaged, accumulated, and instantaneous values, respectively.*

## NLDAS Fields

| Field Name       | Description                              | Units          |
|------------------|------------------------------------------|----------------|
| UGRD             | U wind component at 10 meters            | m s⁻¹          |
| VGRD             | V wind component at 10 meters            | m s⁻¹          |
| TMP              | Air temperature at 2 meters              | K              |
| SPFH             | Specific humidity at 2 meters            | kg/kg          |
| PRES             | Surface pressure                         | Pa             |
| DLWRF            | Downward longwave radiation flux         | W m⁻²          |
| DSWRF            | Downward shortwave radiation flux        | W m⁻²          |
| APCP             | Precipitation hourly total               | kg m⁻²         |
| CAPE             | Convective available potential energy    | J kg⁻¹         |
| PEVAP            | Potential evaporation                    | kg m⁻²         |
| SOILM            | Soil moisture content                    | kg m⁻²         |
| SNOW             | Snow cover                               | %              |
| RAINRATE         | Rainfall rate                            | kg m⁻² s⁻¹     |
| SNOWRATE         | Snowfall rate                            | kg m⁻² s⁻¹     |
| EVAPRATE         | Evaporation rate                         | kg m⁻² s⁻¹     |
| RUNOFF           | Surface runoff                           | kg m⁻²         |
| BASEFLOW         | Baseflow                                 | kg m⁻²         |
| SNOWDEPTH        | Snow depth                               | m              |
| ALBEDO           | Surface albedo                           | %              |
| CANWAT           | Canopy water content                     | kg m⁻²         |
| TSOIL            | Soil temperature                         | K              |
| SWNET            | Net shortwave radiation flux             | W m⁻²          |
| LWNET            | Net longwave radiation flux              | W m⁻²          |
| LHTFL            | Latent heat flux                         | W m⁻²          |
| SHTFL            | Sensible heat flux                       | W m⁻²          |
| GFLUX            | Ground heat flux                         | W m⁻²          |
| SOILW            | Soil wetness                             | fraction       |
| SNOM             | Snow melt                                | kg m⁻²         |
| TSKIN            | Skin temperature                         | K              |
| WIND             | Wind speed                               | m s⁻¹          |

## Comparison of Key Differences

| Field Name | GLDAS Description               | GLDAS Units | NLDAS Description                   | NLDAS Units |
|------------|---------------------------------|-------------|-------------------------------------|-------------|
| Rainf      | Rain precipitation rate         | kg m⁻² s⁻¹  | Rainfall rate                       | kg m⁻² s⁻¹  |
| Snowf      | Snow precipitation rate         | kg m⁻² s⁻¹  | Snowfall rate                       | kg m⁻² s⁻¹  |
| Evap       | Evapotranspiration              | kg m⁻² s⁻¹  | Evaporation rate                    | kg m⁻² s⁻¹  |
| Qs         | Storm surface runoff            | kg m⁻²      | Surface runoff                      | kg m⁻²      |
| Qsb        | Baseflow-groundwater runoff     | kg m⁻²      | Baseflow                             | kg m⁻²      |
| SWE        | Snow depth water equivalent     | kg m⁻²      | Snow cover                          | %           |
| SoilMoist  | Soil moisture                   | kg m⁻²      | Soil moisture content               | kg m⁻²      |