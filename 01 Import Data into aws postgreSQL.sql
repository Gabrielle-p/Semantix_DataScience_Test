/*************************************************************

Date: 20190711

Subject: Semantix Data Science Test - Data Import / Cleansing


Data: Bank Marketing Data Set
https://archive.ics.uci.edu/ml/datasets/bank+marketing

Ran on aws rds Postgresql - Sandbox

***************************************************************/


CREATE SCHEMA Semantix;



/****************************************************

1. Import Bank_Additional Data

*******************************************************/

CREATE TABLE Semantix.Bank_Additional_Raw
(
	age int,
	job varchar(20),
    marital varchar(15),
    education varchar(25),
    default_ varchar(10),
    housing varchar(10),
    loan varchar(10),
    -- related with the last contact of the current campaign:
    contact varchar(15),
    month varchar(3),
    day_of_week varchar(3),
    duration int,
    -- other attributes:
    campaign int,
    pdays int,
    previous int,
    poutcome varchar(15),
    -- social and economic context attributes
    emp_var_rate dec(3,1),
    cons_price_idx dec(6,3),
    cons_conf_idx dec(4,1),
    euribor3m dec(5,3),
    nr_employed dec(6,1),
    y varchar(3) );


/***************************************
Import Data via psql in command line

1. Connect
--Edit the file path for your own data base and user name
psql "host=XXXXXXXXX.rds.amazonaws.com port=5432 dbname=ZZZZZZ user=YYYYYYYY"

2. Load data
--Update file path to your file location
\copy Semantix.Bank_Additional_Raw from '/Documents/Work/Semantix/01 Data Science test/bank-additional/bank-additional-full.csv' with DELIMITER ';' csv header;

3. Returned: COPY 41188

***********************************/

/*Add a row_id column to the table to use as primary key*/
SELECT row_number() over () as row_id, *
INTO Semantix.Bank_Additional
FROM Semantix.Bank_Additional_Raw;

/* Add a primary key to the table*/

ALTER TABLE Semantix.Bank_Additional
ADD PRIMARY KEY (row_id);

--Drop raw import
DROP TABLE Semantix.Bank_Additional_Raw;

--Check table

SELECT *
FROM Semantix.Bank_Additional;


/****************************************************

2. Import Bank_Addtional Data

*******************************************************/


CREATE TABLE Semantix.Bank_Raw
(
age	int,
job	varchar(20),
marital	varchar(15),
education	varchar(25),
default_	varchar(10),
balance	int,
housing	varchar(10),
loan	varchar(10),
contact	varchar(20),
day	int,
month	varchar(3),
duration	int,
campaign	int,
pdays	int,
previous	int,
poutcome	varchar(15),
y	varchar(3)
);


/******************************************************************************************************************************************************
import data via psql in terminal

--Update file path to your file location

\copy Semantix.Bank_Raw from '/Documents/Work/Semantix/01 Data Science test/bank/bank-full.csv' with DELIMITER ';' csv header;

Return: COPY 45211 *why are there more rows than the additional table?

****************************************************************************************************************************************************/

SELECT row_number() over() as row_id, *
INTO Semantix.Bank
FROM Semantix.Bank_Raw; 

ALTER TABLE Semantix.Bank
ADD PRIMARY KEY (row_id);

DROP TABLE Semantix.Bank_Raw; 

/*************************

Compare the two tables

*************************/

SELECT *
FROM semantix.bank_additional ba
LEFT JOIN semantix.bank b
	on b.row_id=ba.row_id
--Rows are not aligned

SELECT ba.age, ba.cnt as cnt_ba, b.cnt as cnt_b
FROM
(
	SELECT age, count(1) as cnt
	FROM semantix.bank_additional
	GROUP BY age) ba 
LEFT JOIN 
(	SELECT age, count(1) as cnt
	FROM semantix.bank 
	GROUP BY age ) b
on ba.age=b.age
ORDER BY ba.age

/* cnt_ba is sometimes higher then cnt_b, therefore not all data from bank.csv is included in bank_additional.csv. The two datasets will be considered as fully independent 

After reading test instructions, only bank.csv should be needed for the test
*/



/*************************************************************************************************************

3. Question 1: 1. Qual profissão tem mais tendência a fazer um empréstimo? De qual tipo?

*************************************************************************************************************/

--Ensure loan only ever takes value yes or no

SELECT
	loan,
	count(1) as cnt
FROM semantix.bank
GROUP BY 
	loan;
--Ok

SELECT
	housing,
	count(1) as cnt
FROM semantix.bank
GROUP BY 
	housing;
--ok


SELECT 
	x.job
	,x.Nb_Housing_Loans
	,1.0*x.Nb_Housing_Loans/x.Nb_Clients_In_Profession as Housing_Loan_Rate
	,x.Nb_Pers_Loans
	,1.0*x.Nb_Pers_Loans/x.Nb_Clients_In_Profession as Pers_Loan_Rate
	,x.Nb_Housing_Loans+x.Nb_Pers_Loans as Nb_Loans_Total
	,1.0*(x.Nb_Housing_Loans+x.Nb_Pers_Loans)/x.Nb_Clients_In_Profession as Overall_Loan_Rate
	,x.Nb_Pers_AnyLoan
	,1.0*x.Nb_Pers_AnyLoan/x.Nb_Clients_In_Profession as Loan_Affinity
	,x.Nb_Clients_In_Profession
FROM 
(
	SELECT 
		job,
		SUM(CASE WHEN housing='yes' THEN 1 ELSE 0 END) as Nb_Housing_Loans,
		SUM(CASE WHEN loan='yes' THEN 1 ELSE 0 END) as Nb_Pers_Loans,
		SUM(CASE WHEN loan='yes' or housing='yes' THEN 1 ELSE 0 END) as Nb_Pers_AnyLoan,
		count(1) as Nb_Clients_In_Profession,
		1.0*SUM(CASE WHEN loan='yes' THEN 1 ELSE 0 END)/count(1) as Loan_Rate
	FROM semantix.bank
	GROUP BY 1
	) x
ORDER BY 7 DESC;


/**********

Answer:

Assuming the dataset is representative of the population (e.g. people from all professions were equally likely to be contacted and figuring in this dataset), Entrepreneurs are the professionals that are most likely to take up a personal loan in relative terms. 

If we include mortgages in the statistics, blue collars are most likely to take up loans.

*******/

SELECT *
FROM semantix.bank;


/*************************

General characteristics

***************************/

--Age

SELECT 
	floor(age/5)*5 as Age,
	count(1) as Cnt
FROM semantix.bank
GROUP BY 1
ORDER BY 1;

--
SELECT 
	education,
	count(1)
FROM semantix.bank
GROUP BY 1;

--
SELECT
	CASE 
		WHEN balance <0 THEN '<0'
		WHEN balance between 0 and 250 THEN '0-250'
		WHEN balance between 250 and 500 THEN '250-500'
		WHEN balance between 500 and 1000 THEN '500-1000'
		WHEN balance between 1000 and 2000 THEN '1000-2000'
		WHEN balance between 2000 and 5000 THEN '2000-5000'
		WHEN balance between 5000 and 10000 THEN '5000-10000'
		WHEN balance > 10000 THEN '>10,000'
	END as Balance_EUR,
	count(1) as cnt
FROM semantix.bank
GROUP BY 1
ORDER BY 1;


SELECT *
FROM semantix.bank
WHERE balance<0


SELECT
	CASE 
		WHEN balance <0 THEN '<0'
		WHEN balance between 0 and 250 THEN '0-250'
		WHEN balance between 250 and 500 THEN '250-500'
		WHEN balance between 500 and 1000 THEN '500-1000'
		WHEN balance between 1000 and 2000 THEN '1000-2000'
		WHEN balance between 2000 and 5000 THEN '2000-5000'
		WHEN balance between 5000 and 10000 THEN '5000-10000'
		WHEN balance > 10000 THEN '>10,000'
	END as Balance_EUR,
	1.0*sum(CASE WHEN housing='yes' THEN 1 ELSE 0 END)/count(1) as mortgage_rate,
	1.0*sum(CASE WHEN loan='yes' THEN 1 ELSE 0 END)/count(1) as pers_loan_rate
FROM semantix.bank
GROUP BY 1
ORDER BY 1;


/*****************************

Question 2

***************************/

SELECT 
	CASE WHEN campaign<=15 THEN campaign ELSE floor(campaign/5)*5 END as Campaign,
	count(1) as Nbr_Clients,
	sum(CASE WHEN Y='yes' THEN 1 ELSE 0 END) as Nbr_Success,
	1.0*sum(CASE WHEN Y='yes' THEN 1 ELSE 0 END)/count(1) as SuccessRate
FROM semantix.bank
GROUP BY 
	CASE WHEN campaign<=15 THEN campaign ELSE floor(campaign/5)*5 END
ORDER BY 1;


/****************************

Question 4

***************************/


SELECT 
	poutcome,
	SUM(CASE WHEN y='yes' THEN 1 ELSE 0 END) as Present_campaign_Y,
	SUM(CASE WHEN y='no' THEN 1 ELSE 0 END) as Present_campaign_N,
	count(1) as Client_Count,
	1.0*SUM(CASE WHEN y='yes' THEN 1 ELSE 0 END)/count(1) as SuccessRate
FROM semantix.bank
GROUP BY poutcome
ORDER BY 5 DESC;
	



