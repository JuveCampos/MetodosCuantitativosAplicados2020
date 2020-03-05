<style>

.bottomright {
  position: absolute;
  bottom: 8px;
  right: 16px;
  font-size: 18px;
  color:red;
} 

.reveal linear {
   color:red;
}

.footer {
    color: black; background: #E8E8E8;
    position: fixed; top: 90%;
    text-align:center; width:100%;
}

/*Controlling the placement of an external image*/
.midcenter {
    position: fixed;
    top: 50%;
    left: 50%;
}

.title-slide {
  background-color: #CBE7A5; /* #EDE0CF; ; #CA9F9D*/
}

</style>

Laboratorio 2
========================================================
author: Juvenal Campos
date: 25/Febrero/2020
autosize: true


First Slide
========================================================

For more details on authoring R presentations please visit <https://support.rstudio.com/hc/en-us/articles/200486468>.

- Bullet 1
- Bullet 2
- Bullet 3

<div class="bottomright"><p>Hola, este es el primer <b>párrafo</b> de mi presentación</p></div>

Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](prez1-figure/unnamed-chunk-2-1.png)
