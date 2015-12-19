inbox:
	Possible qns

1. weekend / ooh vs weekday outcomes. Cord gases, ebl, block complications
2. Weekend / ooh vs weekday anaesthetic technique for cat 1-4 section
3. Time from anaesthesia to surgery for cat 1-4 sections broken down by training grade 
4. Incidence of anaesthetic technique used for cat 1-4 sections. Is this different ooh for cat 1-3?
5. Incidence of complications according to training grade and ooh
6. Anaesthesia to surgery time weekday vs ooh
7. Surgery time weekday vs ooh
8. Has our technique for cat 1 changed (ha vs spinal) over past 5 years?
	
issues:

- things to discuss or raise with Perv
	- missing theatre procedure for 2013-15
		could do a sensitivity analysis to check that are picking up births correctly in the previous data
	- explain data concept
		working with 'births' as the unit of analysis here rather than mothers so this will not capture anaesthetic work for maternal only indications such as retained products, 3rd degree tear etc
	- v low rates of labour epidural in 2009-10 etc ?poor reporting
		- need to work out a metric for checking quality here

continue:
	- fields
		- anaesthetic technique from anaesthetic data
	- produce workload plots
		report per 'shift' (i.e. 12 hours)
	- plot
		- from anaesthetic data
			- labour epidurals over time 
				- by time of day
			- theatre cases over time
				- by time of day

@next:
- extract and clean consultant obs data from theatre database for surgical seniority?
- merge in trainee names
- set up a simple conference abstract

@later:
	theatre data:
		- import theatre name to identify when there are 2 theatres running overnight
		- surgeon name (for seniority) 
	anaesthetic data:
		- import shift (in or out of hours)
	other:
	- remaining merges
		- bloods from drummond street data
		- estimated blood loss from theatre data
	- write a generic merge module for ID (e.g. MRN) plus date?
		you will have to repeat similar merges for
		- census to theatre
		- census to blood results (maternal)
		- census to anaesthetic (where cases don't go theatre)
	



Archive:
	✔ bmi @done (15-12-18 17:57)
	✔ duration of labour (first >> 2nd) @done (15-12-18 22:30)
	✔ apgar @done (15-12-18 22:39)
	✔ cord ph @done (15-12-18 22:39)
	✔ nnu admission @done (15-12-18 22:39)
	- delivery_route @done(2015-12-18)
	- do template merge of theatre data onto census @done(2015-12-12)
		- use MRN + date difference (perfect)
		- use MRN fuzzy?
	- quick plot of @done(2015-12-15)
	- maternal requests only @done(2015-12-16)
		- use merge approach as above	@done(2015-12-15)
			- perfect
			- MRN fuzzy
	- need operating times @done(2015-12-11)
	- complete anaesthetic delay time @done(2015-12-11)
	- import census data @done(2015-11-30)
		- x 5 sheets @done(2015-11-30)
	- create a single unique key @done(2015-11-30)
	- proof of principle @done(2015-12-01)
		- bring in mat_age @done(2015-12-01)
	- stack and merge census data into single data.table @done(2015-12-01)