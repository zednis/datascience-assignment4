# Data

Source and Converted datasets are available under the [CC 1.0 Universal License](https://creativecommons.org/publicdomain/zero/1.0/)

| Dataset Title  | Download Link |
| ------------- | ------------- |
| Boston Food Establishment Inspections (source) | [Food_Establishment_Inspections.csv](https://drive.google.com/open?id=0B9UzvS6jW-Iga1hQdjYwLWI0aVE) |
| Boston Food Establishment Inspections (converted) | [Food_Establishment_Inspections_(converted).csv](https://drive.google.com/open?id=0B9UzvS6jW-IgTy0yRnptX3dfdjg) |
| Chicago Food Inspections (source) | [Food_Inspections.csv](https://drive.google.com/open?id=0B9UzvS6jW-IgZWowOUM2X3ctUG8) |
| Chicago Food Inspections (converted) | [Food_Inspections_(converted).csv](https://drive.google.com/open?id=0B9UzvS6jW-IgLTdIWld1dDlMa2c) |

# Running Transformation and Analysis Scripts

Before running:

Install python3

Then:

to run main.py
```
$ python3 -m pip install numpy
$ python3 -m pip install pandas
$ python3 -m pip install clint
```

to run machine_learning.ipynb
```
$ python3 -m pip install pandas
$ python3 -m pip install pydotplus
$ python3 -m pip install scikit-learn
$ python3 -m pip install matplotlib
$ python3 -m pip install jupyter
```

alternatively you can use the requirements file to install the required libraries

```
$ python3 -m pip install -r requirements.txt
```

Then use main.py to download and covert the datasets

```
$ python3 main.py
```

To run the notebook install and run jupyter notebook (http://jupyter.org/) and open and run machine_learning.ipynb from within the jupyter notebook web app.

To start the jupyter notebook web app do the following after installing jupyter using pip:
```
$ jupyter notebook
```

To run the comment_analysis.R script, you will need to install R (https://cran.r-project.org/) and the necessary packages that are being loaded by the script. The easiest way to execute the script is by installing RStudio (https://www.rstudio.com/), opening the script in RStudio and running it from there.
