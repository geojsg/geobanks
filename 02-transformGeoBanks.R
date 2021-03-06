## FUNCTION CLEANING BANK NAMES BY A FUZZY MATCHING WITH BANK NAMES OF THE REFERENTIAL

MatchBank<-function(n) {

	## Removing special character, convert to upper character and
	## handling heavy change of name (due to real change of name or partial name)

	n %>% gsub("\"","",.) %>% gsub("JSC ","",.) %>% gsub("JS ","",.) %>% 
	gsub(" JSC","",.) %>% gsub("^ ","",.)%>% gsub(" $","",.) %>% toupper() %>%
	gsub("TAOBANK","TAOPRIVATBANK",.) %>% gsub("BTA","BTA SILK ROAD",.) %>%
	gsub("PEOPLE'S  BANK  OF  GEORGIA","LIBERTY BANK",.) %>%
	gsub("UNITED GEORGIAN BANK","VTB BANK-GEORGIA",.) %>%
	gsub("INVESTBANK","CAPITAL BANK",.) -> bval

	## Fuzzy matching by picking th bank names from the referential being the most similar
	banknames[which.min(stringdist(bval,banknames,method="jw"))]
}

## FUNCTION EXTRACTING FEATURES FROM A REPORT

DataFromReport<-function (f) {
	#f<-"banks/ziraat3q_eng.xlsx"
	## Printing filename of the report
	## print(f)

	## Accronym definition:
	## BS: Balance Sheet
	## PL: Profit & Loss
	## RT: Ratio

	## Initializing variables of Bank name and Report Date
	Bank<-NA
	RepDate<-NA

	## Loading Balance Sheet (either sheet "RC" or "RC ")

	BS<-tryCatch({
		read_excel(f,sheet="RC",col_names=F)
		}, error =function (e) {
		read_excel(f,sheet="RC ",col_names=F)
	})

	## Identifying and extracting Total Assets

	colBS<-as.numeric(which(apply(BS,2, function(x) any(grep("^TOTAL ASSETS$",x)))))
	if(length(colBS)==0) print("Error >> Balance sheet: cannot find references column")
	rAsset<-grep("TOTAL ASSETS",BS[,colBS])
	TotAssets<-round(as.numeric(BS[rAsset,colBS+3]),2)

	## Identifying and extracting Net Loans and Deposits (term and on-demand)
	rLoans<-grep("Net Loans",BS[,colBS])
	NetLoans<-round(as.numeric(BS[rLoans,colBS+3]),2)
  NetLoans[is.na(NetLoans)]<-0
	rTimeDeposit<-grep("Time Deposits",BS[,colBS])
	TimeDeposit<-round(as.numeric(BS[rTimeDeposit,colBS+3]),2)
	rDemandDeposit<-grep("Demand Deposits",BS[,colBS])
	DemandDeposit<-round(as.numeric(BS[rDemandDeposit,colBS+3]),2)
	TimeDeposit[is.na(TimeDeposit)]<-0
	DemandDeposit[is.na(DemandDeposit)]<-0
	Deposit<-TimeDeposit+DemandDeposit
	

	## Identifying Bank name and Report date if available on balance sheet

	colBSi<-as.numeric(which(apply(BS,2, function(x) any(grep("^Bank:$",x)))))
	if(length(colBSi)==0) print("Error >> Balance sheet: cannot find info column")
	else {
		rBank<-grep("^Bank:$",BS[,colBSi])
		rDate<-grep("^Date",BS[,colBSi])

		## Extracting Bank name (using cleaning and fuzzy matching function) and report date
		Bank<-MatchBank(BS[rBank,colBSi+1])
		RepDate<-tryCatch({
			as.Date(as.integer(BS[rDate,colBSi+1]),origin="1899-12-30")
			}, warning=function (w) {
			as.Date(parse_date(BS[rDate,colBSi+1]))
		})
	}

	## Loading Profit & loss statement (either sheet "RI" or "RI ")

	PL<-tryCatch({
		read_excel(f,sheet="RI",col_names=F)
		}, error =function (e) {
		read_excel(f,sheet="RI ",col_names=F)
	})

	## Identifying item name column in the P&L sheet

	colPL<-as.numeric(which(apply(PL,2, function(x) any(grep("^Interest Income$",x)))))
	if(length(colPL)==0) print("Error >> Profit & Loss sheet: cannot find references column")

	## Identifying and extracting Personnel Expenses

	rStaff<-grep("Personnel Expenses",PL[,colPL])
	if(length(rStaff)==0) print("Error >> Profit & Loss sheet: cannot find row Staff expenses")
	StaffExp<-round(as.numeric(PL[rStaff,colPL+3]),2)

	## Identifying and extracting Net Income

	rNet<-grep("^Net Income$",PL[,colPL])
	if(length(rNet)==0) print("Error >> Profit & Loss sheet: cannot find row Net Income")
	NetIncome<-round(as.numeric(PL[rNet,colPL+3]),2)

	## If Bank name or Report date could not be found on balance sheet, identifying them on P&L

	if (is.na(Bank) | is.na(RepDate)) {
		colPLi<-as.numeric(which(apply(PL,2, function(x) any(grep("^Bank:$",x)))))
		if(length(colPLi)==0) print("Error >> Profit & loss: cannot find info column")
		else {
			rBank<-grep("^Bank:$",PL[,colPLi])
			rDate<-grep("^Date",PL[,colPLi])

			if(is.na(Bank)) Bank<-MatchBank(PL[rBank,colPLi+1])
			if(is.na(RepDate)) {
				RepDate<-tryCatch({
					as.Date(as.integer(PL[rDate,colPLi+1]),origin="1899-12-30")
					}, warning=function (w) {
					as.Date(parse_date(PL[rDate,colPLi+1]))
				})
			}
		}
	}

	## In case, no date could be found, raise an error and stop the process.
	if (is.na(RepDate)) stop("Date not defined")

	## Manage exceptions (wrong date filled in report)
	if (grepl("pasha3q_eng.xlsx$",f)) RepDate<-as.Date("2014-09-30")
	if (grepl("decree145iba062009e.xls$",f)) RepDate<-as.Date("2009-06-30")
	if (grepl("decree145kor032009e.xls$",f)) RepDate<-as.Date("2009-03-31")
	#if (grepl("decree_145lib102012e.xls$",f)) RepDate<-as.Date("2012-09-30")
	if (grepl("decree145tao062009e.xls$",f)) RepDate<-as.Date("2009-06-30")
	if (grepl("bpgqi.xlsx$",f)) RepDate<-as.Date("2014-03-31")
	
	
	## Generate a 1-row dataframe including Banks' feature of a quarter report
	data.frame(Bank=Bank, RepDate=RepDate, TotalAssets=TotAssets, StaffExpenses=StaffExp, NetIncome=NetIncome, NetLoans=NetLoans, Deposit=Deposit)

}


## FUNCTION TO GATHER THE FEATURES FROM ALL REPORTS AND EXPORT TO CSV

TableBank<-function(source) {
	reports<-list.files(source)
  register<-NULL
	exceptions<-c("nbg3.2.2peoplesbank30.06.2007_2eng.xls","progress_3q_11_11.xlsx")

	for (report in reports) {

		if (sum(sapply(exceptions, function(x) grepl(paste(x,"$",sep=""),report)))==0) {
			if (exists("register")) {
				register<-rbind(register,DataFromReport(paste(source,report,sep="/")))
			} else { 
				register<-DataFromReport(paste(source,report,sep="/")) 
			}
		}
	}
	print(paste("Variable 'register' created from ",length(reports)," reports"))
	register
	
}


## RUNNING

## Loading libraries
library(magrittr)
library(readxl)
library(stringdist)
library(parsedate)

## directories (without "/" at the end)
dir_reports<-"Banks"
dir_banknames<-"."
dir_output<-"."

## Get bank names
bankn<-read.csv(paste(dir_banknames,"banknames.csv",sep="/"),colClasses="character")
#bankn<-read.csv("banknames.csv",colClasses="character")
banknames<-bankn$Names

## Launch the generation of the table and its export to a CSV file
register<-TableBank(dir_reports)

## Converting date to quarter of year
register$Quarter<-paste(as.integer(format(register$RepDate,"%Y")),as.integer(format(register$RepDate,"%m"))%/%3,sep="Q")

## Calculate total market (sum of all banks) of Net Loans and Deposit per quarter
agg<-aggregate(cbind(NetLoans, Deposit) ~ Quarter, data=register,sum)
names(agg)[2:3]<-c("TotalLoans","TotalDeposit")

## Add total loans and deposits market for each banks quarter report (doing a left-join)
register2<-merge(register,agg,by.x="Quarter", by.y="Quarter", all.x=TRUE)

## Calculating the market shares (Loan & Deposit) of each Bank per quarter
register2$DepositShare<-round(100*register2$Deposit/register2$TotalDeposit,2)
register2$LoanShare<-round(100*register2$NetLoans/register2$TotalLoans,2)

## Exporting the table Keeping only features needed for future charts
write.csv(register2[,-c(3,7,8,9,10)],paste(dir_output,"GeoBanks.csv",sep="/"),row.names=F)
print (paste("File created: ",dir_output,"/","GeoBanks.csv",sep=""))
