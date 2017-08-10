# lumpR

Landscape Unit Mapping Program for R


## DESCRIPTION

This project deals with an R-package called "lumpR". The package provides functions for a semi-automated approach of the delineation and description of landscape units and partition into terrain components. It can be used for the pre-processing of semi-distributed large-scale hydrological and erosion models using catena-representation (WASA-SED, CATFLOW). It is closely connected to and uses functionalities of GRASS GIS. Additional pre-processing tools beyond the scope of the original LUMP algorithm are included.


## INSTALLATION

* command line installation:

```R
install.packages("devtools") 
library(devtools)
install_github("tPilz/lumpR")
```

* from zip/tar:
	* download zip/tar from github: [>LINK<](https://github.com/tpilz/lumpR/releases)
	* install via R-GUI


## MORE INFORMATION

Have a look at our wiki for more detailed information: [>LINK<](https://github.com/tpilz/lumpR/wiki)


## FEEDBACK and BUGS

Feel free to comment via github issues: [>LINK<](https://github.com/tpilz/lumpR/issues)


## LICENSE

lumpR is distributed under the GNU General Public License version 3 or later. The license is available in the file `GPL-3` of lumpR's source directory or online: [>LINK<](http://www.gnu.org/licenses/gpl.html)

## NOTE

This package was also known as LUMP and has been renamed by Jan 9th 2017 to distinguish it from the LUMP algorithm published by Francke et al. (2008).


## REFERENCES

A paper describing lumpR along with an example study was published in GMD:

[Pilz, T., Francke, T., and Bronstert, A.: lumpR 2.0.0: an R package facilitating landscape discretisation for hillslope-based hydrological models, Geosci. Model Dev., 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017, 2017.](https://www.geosci-model-dev.net/10/3001/2017/gmd-10-3001-2017.html)

See also the accompanying github repository: https://github.com/tpilz/lumpr_paper


For the original LUMP algorithm see:

[Francke, T., Güntner, A., Mamede, G., Müller, E. N., and Bronstert, A.: Automated catena-based discretization of landscapes for the derivation of hydrological modelling units, Int. J. Geogr. Inf. Sci., 22, 111-132, doi:10.1080/13658810701300873, 2008.](http://www.tandfonline.com/doi/abs/10.1080/13658810701300873)
