---
title: "Comparing Georgian Banks"
author: "JS Gourdet"
output: html_document
---

<center>
# Comparing Georgian Banks
### *An example of web scraping, data wrangling and data visualization with R*

by JS Gourdet  

<img src="http://www.iset.ge/blog/wp-content/uploads/2013/05/photo-v2-640x240.png" heigth=120 width=320 alt="Georgian Banks">
</center>

## 1. Objectives

Quarterly, financial reports of Georgian Banks are uploaded on the website of the National Bank of Georgia.  
A lot of semi-structured financial data are therefore publicly available.
Let's use them to compare Georgian Banks from different points of view.    
  
But how to exploit, transform and represent these data from the National Bank of Georgia website to a user-friendly charts ?  
This is the challenge that this document is addressing using the tool R. 

**Disclaimer**: The objective of this document is not to provide an accurate financial analysis of the Georgian Banks, 
but just the technical steps to show how to leverage insights from available raw data.

## 2. Input

Data source for the analysis is the page "*Financial Indices of Commercial Banks*" of the National Bank of Georgia:
https://www.nbg.gov.ge/index.php?m=404&lng=eng

## 3. Outputs: Motion charts (4D)

### 3.1. Evolution of Banks' Net Income, Total Assets and Personnel over time

The purpose of this graph is to show the evolution over time of the Banks' net income (from profit & loss statement) 
according to their total assets (balance sheet) and sizing them (bubbles) by the size of their personnel (using personnel expenses from P&L).

However, as figures of profit & loss statement are reset every year, it does not make sense to each quarterly reports over years, otherwise the chart would 
look like a sawtooth-shaped chart. So only 1 quarter (the 3rd as the 4th is not available) is picked for each year. Then other quarters are extrapolated (of course not very accurate, but enough for this technical excerice).

**User guide:**  
1. As the variable ***Size*** on the chart cannot be set automatically (bug), set it manually to the value "***StaffExp***"  
2. Press the button play (arrow) to visualize the motion chart.  
3. You can change axis if you want but also visualize other chart using other tabs (top-right corner)  



<!-- MotionChart generated in R 3.2.0 by googleVis 0.5.8 package -->
<!-- Sat Jun 06 23:30:16 2015 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataMotionChartID27f87d412fd5 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "KOR STANDARD BANK",
"2007Q3",
174714505,
2652308.91,
3396858.87 
],
[
 "THE INTERNATIONAL BANK OF AZERBAIJAN-GEORGIA",
"2014Q3",
128150777.6,
2539046.95,
923025 
],
[
 "THE INTERNATIONAL BANK OF AZERBAIJAN-GEORGIA",
"2013Q3",
134652715.2,
889705.64,
957654.54 
],
[
 "BASISBANK",
"2013Q3",
348138060,
6498013.44,
4684525.74 
],
[
 "BASISBANK",
"2014Q3",
609696047.6,
11394481.77,
5530881.49 
],
[
 "BANK OF GEORGIA",
"2013Q3",
5385578769,
77639626.02,
72145744.95 
],
[
 "BANK OF GEORGIA",
"2014Q3",
6049363372,
115938682.4,
80318500.01 
],
[
 "SILK ROAD BANK",
"2013Q3",
126832551.8,
-3196107.31,
4101102.67 
],
[
 "SILK ROAD BANK",
"2014Q3",
123380427.1,
-4191741.78,
3685396.36 
],
[
 "CAPITAL BANK",
"2014Q3",
47677268,
-3794858,
2066459 
],
[
 "CARTU BANK",
"2013Q3",
477530632,
48037236,
5738941 
],
[
 "CARTU BANK",
"2014Q3",
681969282,
25565156,
5550572 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2013Q3",
17603673,
58130,
169027 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2014Q3",
27372597,
149029,
424633 
],
[
 "BANK CONSTANTA",
"2013Q3",
321379019.2,
10302936.41,
13860969.19 
],
[
 "BANK CONSTANTA",
"2014Q3",
414578577.3,
12524610.33,
16229996.54 
],
[
 "BASISBANK",
"2008Q3",
108729411,
1502723.44,
3095209 
],
[
 "BASISBANK",
"2009Q3",
122813000,
-2384569.27,
3241011 
],
[
 "BANK OF GEORGIA",
"2011Q3",
4174281308,
86606002,
61674573 
],
[
 "BANK OF GEORGIA",
"2008Q3",
2706451816,
-4876360,
53609081 
],
[
 "BANK OF GEORGIA",
"2009Q3",
2647087060,
-9858267,
45727939 
],
[
 "SILK ROAD BANK",
"2011Q3",
114329022.4,
428074.61,
3297753.53 
],
[
 "CARTU BANK",
"2011Q3",
532444297,
14181339,
5384189 
],
[
 "BANK CONSTANTA",
"2009Q3",
61192518.37,
660332.84,
3956100.59 
],
[
 "BANK CONSTANTA",
"2011Q3",
123712248.9,
2246030,
6535629.21 
],
[
 "BANK CONSTANTA",
"2010Q3",
79745760.47,
1230780.33,
4917373.06 
],
[
 "HALYK BANK GEORGIA",
"2011Q3",
37289233,
-231358,
1643195 
],
[
 "THE INTERNATIONAL BANK OF AZERBAIJAN-GEORGIA",
"2011Q3",
100428086.8,
1052529.59,
955258.07 
],
[
 "THE INTERNATIONAL BANK OF AZERBAIJAN-GEORGIA",
"2008Q3",
38026364,
1011883.22,
550667.53 
],
[
 "CAPITAL BANK",
"2011Q3",
23492081,
-640630,
900925 
],
[
 "KOR STANDARD BANK",
"2011Q3",
362435905.6,
2346099.35,
6545310.27 
],
[
 "KOR STANDARD BANK",
"2010Q3",
297968370,
-2704756.57,
6648898.67 
],
[
 "LIBERTY BANK",
"2008Q3",
357093321.4,
-1095982,
19670911 
],
[
 "PROCREDIT�BANK",
"2011Q3",
837289009.8,
11327760.72,
23749762.5 
],
[
 "PROGRESS BANK",
"2011Q3",
14115202.05,
115683.98,
893188.02 
],
[
 "BANK REPUBLIC",
"2011Q3",
686038365.3,
-8921293.24,
18887172.86 
],
[
 "SILK ROAD BANK",
"2007Q3",
147838997.4,
2261817.48,
1839454.87 
],
[
 "PRIVATBANK",
"2011Q3",
391372108.6,
3557186.03,
16241296 
],
[
 "TBC BANK",
"2011Q3",
2979538033,
69922210,
46508408 
],
[
 "TBC BANK",
"2008Q3",
1953765001,
3142812,
33672717 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2011Q3",
15630874,
1957695,
191794 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2008Q3",
13990501,
-364226,
243167 
],
[
 "VTB BANK-GEORGIA",
"2011Q3",
392813560.9,
15756416,
14094587 
],
[
 "VTB BANK-GEORGIA",
"2008Q3",
411345953.6,
-27374022,
12688923 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2011Q3",
23034660,
571203,
603935 
],
[
 "BASISBANK",
"2010Q3",
140390545.5,
1895410.02,
3761651.75 
],
[
 "BANK OF GEORGIA",
"2010Q3",
3375929460,
93339100,
55303003 
],
[
 "SILK ROAD BANK",
"2010Q3",
100325601.8,
-9520170.49,
3242151.58 
],
[
 "SILK ROAD BANK",
"2009Q3",
142556568.4,
-3562606.56,
4170091.99 
],
[
 "CARTU BANK",
"2010Q3",
515549528,
9501815,
5500683 
],
[
 "BANK CONSTANTA",
"2008Q3",
63535682.54,
-2535186.34,
5052948.78 
],
[
 "HALYK BANK GEORGIA",
"2010Q3",
31108358,
-2142407,
1486867 
],
[
 "HALYK BANK GEORGIA",
"2009Q3",
29444472,
-2114173,
1862844 
],
[
 "THE INTERNATIONAL BANK OF AZERBAIJAN-GEORGIA",
"2010Q3",
46535971.67,
86934.69,
774054.43 
],
[
 "THE INTERNATIONAL BANK OF AZERBAIJAN-GEORGIA",
"2009Q3",
34887727.38,
-598356.02,
662004 
],
[
 "CAPITAL BANK",
"2008Q3",
24448351,
332428,
1291217 
],
[
 "CAPITAL BANK",
"2009Q3",
9609851,
-6211826,
1422065 
],
[
 "CAPITAL BANK",
"2010Q3",
15398529,
416735,
1000820 
],
[
 "CARTU BANK",
"2008Q3",
415217284,
4129431,
5437063 
],
[
 "CARTU BANK",
"2009Q3",
471467292,
7160344,
4482519 
],
[
 "KOR STANDARD BANK",
"2008Q3",
250707439.7,
-7049911.76,
6339219.21 
],
[
 "KOR STANDARD BANK",
"2009Q3",
240386917.5,
-3091081.44,
7037302 
],
[
 "PROCREDIT�BANK",
"2008Q3",
606344779.7,
8355210.3,
19539958.02 
],
[
 "PROCREDIT�BANK",
"2009Q3",
686376309.8,
6793487.01,
22566813.37 
],
[
 "PROGRESS BANK",
"2008Q3",
13037095,
-384353,
808033 
],
[
 "PROGRESS BANK",
"2009Q3",
12655791.21,
-485051.87,
849020 
],
[
 "PROCREDIT�BANK",
"2010Q3",
828460129.2,
11195036.85,
23033024.76 
],
[
 "PROGRESS BANK",
"2010Q3",
13648484.02,
290792.89,
786098 
],
[
 "BANK REPUBLIC",
"2008Q3",
647586339.8,
1656208.81,
15645397.59 
],
[
 "BANK REPUBLIC",
"2009Q3",
747412597.1,
-1190328.92,
17315988.66 
],
[
 "BANK REPUBLIC",
"2010Q3",
935747168.3,
-27380564.13,
18369333.6 
],
[
 "SILK ROAD BANK",
"2008Q3",
140428063.9,
1965437.57,
3645269.22 
],
[
 "PRIVATBANK",
"2010Q3",
202313825.3,
-5512051,
10361891 
],
[
 "PRIVATBANK",
"2008Q3",
274957519.6,
-8073016.18,
10431650 
],
[
 "PRIVATBANK",
"2009Q3",
151110827.9,
-41331774,
10299455 
],
[
 "TBC BANK",
"2010Q3",
2042598132,
29697498,
35306854 
],
[
 "TBC BANK",
"2009Q3",
1730968721,
2417052.54,
31984952.17 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2010Q3",
14071194,
450921,
175705 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2009Q3",
14481879,
-84862,
192524 
],
[
 "VTB BANK-GEORGIA",
"2010Q3",
353361030.7,
-7734245,
12533067 
],
[
 "VTB BANK-GEORGIA",
"2009Q3",
371467775.5,
-12551526,
12492049 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2008Q3",
25067754,
1933712,
375946 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2009Q3",
22939431,
770916,
509283 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2010Q3",
24457123,
157451,
526775 
],
[
 "FINCA BANK GEORGIA",
"2014Q3",
152008143.4,
4918217.55,
10599373.85 
],
[
 "HALYK BANK GEORGIA",
"2014Q3",
151059110.8,
1449031,
2783965 
],
[
 "HALYK BANK GEORGIA",
"2013Q3",
106712505,
470284,
2166909 
],
[
 "CAPITAL BANK",
"2013Q3",
30787319,
-1879129,
2311103 
],
[
 "TURKEY ISBANK A.S BATUMI BRANCH",
"2013Q3",
25041756.98,
-273488.92,
498435.43 
],
[
 "TURKEY ISBANK A.S BATUMI BRANCH",
"2014Q3",
54454945.3,
-85875.05,
1203303.99 
],
[
 "KOR STANDARD BANK",
"2014Q3",
549228868.5,
-8718452.36,
6892442 
],
[
 "KOR STANDARD BANK",
"2013Q3",
451542623.7,
5449191.36,
7356148 
],
[
 "LIBERTY BANK",
"2014Q3",
1571762742,
17417938,
34830022 
],
[
 "LIBERTY BANK",
"2013Q3",
1297933906,
18697117,
28707713 
],
[
 "THE INTERNATIONAL BANK OF AZERBAIJAN-GEORGIA",
"2007Q3",
19389924.05,
-638996.7,
224322 
],
[
 "BASISBANK",
"2006Q3",
76238428,
1681528,
1486122 
],
[
 "BASISBANK",
"2007Q3",
100335007,
2605874,
2056705 
],
[
 "BANK OF GEORGIA",
"2006Q3",
844578684,
13895113,
19681213 
],
[
 "BANK OF GEORGIA",
"2007Q3",
2267467983,
35126371,
42733266 
],
[
 "SILK ROAD BANK",
"2006Q3",
50536322.96,
1074243.42,
668695.45 
],
[
 "CARTU BANK",
"2006Q3",
264946600,
8613952,
2797079 
],
[
 "CARTU BANK",
"2007Q3",
386681714,
10288059,
4218128 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2006Q3",
14113117,
-19020,
59143 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2007Q3",
12897931,
56219,
154714 
],
[
 "CAPITAL BANK",
"2006Q3",
25320317,
611768,
890949 
],
[
 "CAPITAL BANK",
"2007Q3",
34404496,
828740,
1139596 
],
[
 "LIBERTY BANK",
"2006Q3",
111482294,
2865481,
7133857 
],
[
 "LIBERTY BANK",
"2007Q3",
232348152,
934249,
11675732 
],
[
 "PROCREDIT�BANK",
"2006Q3",
417019990.4,
4110815.69,
10928865.57 
],
[
 "PROCREDIT�BANK",
"2007Q3",
498876308.4,
7276916.16,
15017266.06 
],
[
 "BANK REPUBLIC",
"2006Q3",
373143160.4,
8120648.94,
5600561 
],
[
 "BANK REPUBLIC",
"2007Q3",
441915338.5,
3971435.45,
9988338.49 
],
[
 "KOR STANDARD BANK",
"2006Q3",
113935743.2,
822146.58,
1150205 
],
[
 "PRIVATBANK",
"2006Q3",
21042663,
576044,
728741 
],
[
 "PRIVATBANK",
"2007Q3",
89842302.2,
-1015495,
2038678 
],
[
 "TBC BANK",
"2006Q3",
907949543.6,
16802605.36,
12832576 
],
[
 "TBC BANK",
"2007Q3",
1663377411,
16925416,
20044461 
],
[
 "VTB BANK-GEORGIA",
"2006Q3",
473876164.7,
4866724,
7823027 
],
[
 "VTB BANK-GEORGIA",
"2007Q3",
516288007.1,
2914672,
10002135 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2006Q3",
11708250,
470477,
345417 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2007Q3",
19559298,
650716,
413881 
],
[
 "PASHA BANK GEORGIA",
"2013Q3",
35826511.37,
-1776577.9,
1124048.28 
],
[
 "PASHA BANK GEORGIA",
"2014Q3",
132040708.4,
-7945484.57,
1512306.95 
],
[
 "PRIVATBANK",
"2014Q3",
605653333.4,
713667,
14657903 
],
[
 "PRIVATBANK",
"2013Q3",
486417627.6,
26036602,
13203100 
],
[
 "PROCREDIT�BANK",
"2013Q3",
1001665187,
12321896.81,
21585323.63 
],
[
 "PROCREDIT�BANK",
"2014Q3",
1051627329,
13482578.81,
18784721.72 
],
[
 "PROGRESS BANK",
"2014Q3",
80424867.49,
-182921.52,
2028525 
],
[
 "BANK REPUBLIC",
"2013Q3",
890813139.4,
18687927.4,
21678623.07 
],
[
 "BANK REPUBLIC",
"2014Q3",
1081494153,
26013725.75,
21151042.22 
],
[
 "TBC BANK",
"2013Q3",
3659793845,
45384429.74,
50409288.53 
],
[
 "TBC BANK",
"2014Q3",
4611324080,
86109758.21,
62857577.48 
],
[
 "BASISBANK",
"2012Q3",
211198602.9,
304492.2,
4359919.2 
],
[
 "BANK OF GEORGIA",
"2012Q3",
5177594727,
109052544,
66393379 
],
[
 "CARTU BANK",
"2012Q3",
307529056,
-75123042,
6736707 
],
[
 "BANK CONSTANTA",
"2012Q3",
218561525.5,
6308271.86,
9154905.88 
],
[
 "HALYK BANK GEORGIA",
"2012Q3",
65033757.5,
-839274,
1875075 
],
[
 "THE INTERNATIONAL BANK OF AZERBAIJAN-GEORGIA",
"2012Q3",
128674858.3,
4348054.81,
1096972.23 
],
[
 "CAPITAL BANK",
"2012Q3",
33527126,
3725244,
1636061 
],
[
 "KOR STANDARD BANK",
"2012Q3",
399977575,
3704231.57,
6927262.84 
],
[
 "PRIVATBANK",
"2012Q3",
495757769,
-6141974,
12105959 
],
[
 "PROCREDIT�BANK",
"2012Q3",
1043116734,
16569671.48,
23930132.1 
],
[
 "PROGRESS BANK",
"2012Q3",
18083122.88,
-728004.84,
1404371 
],
[
 "BANK REPUBLIC",
"2012Q3",
771792831.8,
13025409.72,
18277655.84 
],
[
 "SILK ROAD BANK",
"2012Q3",
130668449.9,
-3640193.99,
3993446.68 
],
[
 "TBC BANK",
"2012Q3",
3604451897,
52256321.83,
46219374.95 
],
[
 "CAUCASUSDEVELOPMENT BANK - GEORGIA",
"2012Q3",
15159270,
-483769,
276917 
],
[
 "VTB BANK-GEORGIA",
"2012Q3",
476151464.4,
8366785,
16127741 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2012Q3",
24140370,
139444,
572438 
],
[
 "TURKEY ISBANK A.S BATUMI BRANCH",
"2012Q3",
12297121.76,
-100250.57,
75276 
],
[
 "VTB BANK-GEORGIA",
"2013Q3",
766046906,
6967788,
19863769 
],
[
 "VTB BANK-GEORGIA",
"2014Q3",
945308881.2,
10177290,
21712571 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2013Q3",
43551663,
-203304,
609110 
],
[
 "ZIRAAT BANKASI A.S.�TBILISI BRANCH",
"2014Q3",
46941437,
235805,
828641 
] 
];
data.addColumn('string','Bank');
data.addColumn('string','Quarter');
data.addColumn('number','TotAssets');
data.addColumn('number','NetIncome');
data.addColumn('number','StaffExp');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartMotionChartID27f87d412fd5() {
var data = gvisDataMotionChartID27f87d412fd5();
var options = {};
options["width"] =    600;
options["height"] =    400;
options["state"] = "{\"colorOption\":\"_UNIQUE_COLOR\"};";

    var chart = new google.visualization.MotionChart(
    document.getElementById('MotionChartID27f87d412fd5')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "motionchart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartMotionChartID27f87d412fd5);
})();
function displayChartMotionChartID27f87d412fd5() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartMotionChartID27f87d412fd5"></script>
 
<!-- divChart -->
  
<div id="MotionChartID27f87d412fd5" 
  style="width: 600; height: 400;">
</div>

## 4. Intermediary Steps

### Step 1: Downloading Banks' Quarterly Reports

**Input**: Webpage "Financial Indices of Commercial Banks" of the National Bank.

**Sub-steps:**
1. Extracting the list of Georgian Banks's names (to use as referential for further steps).
2. Extracting all the URLs of the quarterly reports
3. Downloading all the quarterly reports

**Outputs:**
* CSV file with list of banks
* Excel files of all Banks' quaterly reports 

See source code: [01-harvestGeoBanks.R](http://github.com/geojsg)

### Step 2: Transforming raw data into structured data

**Input:** 
- CSV file with list of banks (generated from step1)
- Excel files of all Banks' quaterly reports (generated from step1)

**Sub-steps:**  
For each reports, extract following fields :
1. Bank name (handling exceptions of renamed banks and using a fuzzy matching as names are sometimes partial or with typos)  
2. Report Date (using parsing as not all the time in same format)
3. NetIncome and Personnel expenses from P&L
4. TotalAssets from balance sheet

**Outputs:**
- CSV file including selected features (e.g. Total Assets, ...) per bank and per quarter.

See source code: [02-transformGeoBanks.R](http://github.com/geojsg)

### Step 3: Generating the charts embedded in a HTML file

**Input:**
- CSV file including selected features (e.g. Total Assets, ...) per bank and per quarter. (generated from step2)

**Sub-steps:**  
1. Creating report (this HTML page) in Rmarkdown  
2. Filtering only banks' figures from Quarter 3  
3. Building the chart with Google Chart (googleVis)  
4. Displaying the chart embedded in the page.  

**Output:**
- HTML file (this page) with embedded motion charts.

See source code: [03-GeoBanks.Rmd](http://github.com/geojsg)

## 5. Lessons Learned

- Unfortunately, bank's financial data of quarter IV (most important) of each year is not available as data are uploaded in pdf. We suggest to the National Bank to provide data in Excel format as well.  
- Google Motion Chart seems to have some bugs: when bubbles' color is set to "_UNIQUE", the the feature for te size of bubbles is no more considered.
- Reports in Excel could mean data are semi-structured as formated by a template. However, data quality was not so good (see below issues) which required to add more codes to handle exceptions or make fuzzy matching and parsing.


### Summary of Data Quality issues

- 2013-Q3 report of Progress Bank not available  
- 2007-Q2 report of Liberty Bank (People's Bank) not available as, instead of it, was uploaded report of Bank Republic  
- In 2014-Q3 report of Pasha Bank, report date written is the one of the previous year (2013-Q3)
- Report Date and Bank name are not all the time written on each sheet.
- Dates are not in same date format (Excel date format) in several reports.  
- Names of the Excel sheets are not exactly the same for some reports. For example, there can be found, sheets named "RC " instead of "RC".  
