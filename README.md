# **Cobol Copy Maker**

## **Description**

This is a small HTML page linked with a bit of javascript which allows you to generates a cobol Copy from a CSV File

## **Installation**

All you need is the [html page](CopyMaker.html), the [css style file](./src/CopyMaker.css) and the [javascript file](./src/main.js).

In the [resources folder](./ressources/), you will also find a [spreadsheet](resources/Copy.ods) allowing you to easily create a `.csv` file with the corresponding format :
```CSV
LEVEL;PARAMETER-NAME;TYPE;DESCRITPION
```
or
```CSV
05;DATA1;This is a description;PIC X(10)
05;DATA2;This is another description;PIC 9(05)
```

NB : More examples are available [here](resources/testCopy1.csv) and [here](resources/testCopy2.csv).

NB2 : The CSV doesn't require any headers. Please make sure there is any in your file.

## **Potential Errors**

- Make sure to use an up-to-date browser to allow the javascript code to work properly
- The `Copy to keyboard` button may not work. It's because this option uses a deprecated method, which has no simple alternative. I guess you will have to do with it :)

## **Authors**

* Sébastien HERT


Have fun and enjoy !