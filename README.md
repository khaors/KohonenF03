# KohonenF03

A fortran 2003 library for Self-Organizing Maps

## Compilation

Currently there are two programs implemented in the library _som_train_ and _som_predict_ which can be compiled using

```
make som_train
```

and

```
make som_predict
```

## Testing

A simple example is included in the repository in which the goal is to find the clusters present in the _iris_ dataset. This dataset is included in the _iris.dat_ file where the samples have been properly scaled in the range between 0 and 1. The input parameters of the program _som_train_ are specified in a parameter file. 

```
som_train som_train.par
```

## Licensing, Authors, Acknowledgements
KohonenF03 was written by Oscar Garcia-Cabrejo and is distributed under the [3-Clause BSD license](https://github.com/khaors/KohonenF03/blob/master/LICENSE). 

You can cite KohonenF03 in research publications and reports as follows:
* Garcia-Cabrejo, O. (2019). ***KohonenF03: A library for Self-Organizing Maps in Fortran 2003***. https://github.com/khaors/KohonenF03. Accessed: *day month year*.

BibTeX entry:
```
@misc{Garcia-Cabrejo19,
 author = {Garcia-Cabrejo, O},
 title 	= {{KohonenF03: A library for Self-Organizing Maps in Fortran 2003}},
 year 	= 2019,
 howpublished = {\url{https://github.com/khaors/KohonenF03}},
 note 	= {Accessed: day month year}
}
```