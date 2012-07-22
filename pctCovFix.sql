--first goal is to merge the two, selecting their respective ev.grams

select tblvegtransectpctcover.transectid,tblvegtransectpctcover.habitattype,tblvegtransectpctcover.gram,completecover.gram as newgram 
from tblvegtransectpctcover inner join completecover on tblvegtransectpctcover.transectid = completecover.transectid and tblvegtransectpctcover.habitattype = completecover.habitattype;

--next select the ones that don't match
select tid,hab,start,oldgram,newgram from 
	(select tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.start as start,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.gram as oldgram,completecover.gram as newgram from 
		(tblvegtransectpctcover inner join completecover 
			on tblvegtransectpctcover.transectid = completecover.transectid and tblvegtransectpctcover.habitattype = completecover.habitattype and tblvegtransectpctcover.start = completecover.start))
	where oldgram not like newgram;

--need this for all functional types. also, make sure to include start or end distance in case the order of entry was switched.
--types are sphag, non_sphag, lichen,gram,forb,gram,dec_gram,tree,eq,dw,bare

select tid,hab,start,oldgram,newgram from 
	(select tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.start as start,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.gram as oldgram,completecover.gram as newgram from 
		(tblvegtransectpctcover inner join completecover 
			on tblvegtransectpctcover.transectid = completecover.transectid and tblvegtransectpctcover.habitattype = completecover.habitattype and tblvegtransectpctcover.start = completecover.start))
	where oldgram not like newgram;

select tid,hab,start,oldbare,newbare from 
	(select tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.start as start,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.bare as oldbare,completecover.bare as newbare from 
		(tblvegtransectpctcover inner join completecover 
			on tblvegtransectpctcover.transectid = completecover.transectid and tblvegtransectpctcover.habitattype = completecover.habitattype and tblvegtransectpctcover.start = completecover.start))
	where oldbare not like newbare;

--so the problem is pervasive through the pct covers that were transferred. double-checking the non-vasculars is a separate problem.
--first you need an insert statement that will replace the current value in yrb with the correct one from 2011yrb when the two don't match but transectid, habitattype,start, and end do.

--select first, assume 2011covtable moved over as Cov2011
--try this on a copy first
--do a left join on the current table, extracting the primary key but getting the correct cover value, then replace that cover value in the original table based on primary key.
--perCovAutonumber is pkey in vegtransectpctcover


update tblvegtransectpctcover set eg_shrub = compare.newshrub where percovautonumber = compare.pkey (select pkey,tid,hab,start,oldshrub,newshrub from (select tblvegtransectpctcover.percovautonumber as pkey,tblvegtransectpctcover.transectid as tid,tblvegtransectpctcover.habitattype as hab,tblvegtransectpctcover.start as start,tblvegtransectpctcover.eg_shrub as oldshrub,cov2011.eg_shrub as newshrub from 
	(tblvegtransectpctcover inner join cov2011 
			on tblvegtransectpctcover.transectid = cov2011.transectid and tblvegtransectpctcover.habitattype = cov2011.habitattype and tblvegtransectpctcover.start = cov2011.start))
	where oldshrub not like newshrub) as compare where compare.pkey = 2298;


--also need to fix the weird cover class designations
--done below with one update statement. booyah.

update tblvegtransectpctcover set sphag = switch(sphag in ('','-','0'),'NA',sphag = '1.5','1',sphag = '6','5',sphag = 'DNR','-1',sphag not in ('','-','0','1.5','6','DNR'),sphag),

non_sphag = switch(non_sphag in ('','-','0'),'NA',non_sphag = '1.5','1',non_sphag = '6','5',non_sphag = 'DNR','-1',non_sphag not in ('','-','0','1.5','6','DNR'),non_sphag),

lichen = switch(lichen in ('','-','0'),'NA',lichen = '1.5','1',lichen = '6','5',lichen = 'DNR','-1',lichen not in ('','-','0','1.5','6','DNR'),lichen),

gram = switch(gram in ('','-','0'),'NA',gram = '1.5','1',gram = '6','5',gram = 'DNR','-1',gram not in ('','-','0','1.5','6','DNR'),gram),

forb = switch(forb in ('','-','0'),'NA',forb = '1.5','1',forb = '6','5',forb = 'DNR','-1',forb not in ('','-','0','1.5','6','DNR'),forb),

dec_shrub = switch(dec_shrub in ('','-','0'),'NA',dec_shrub = '1.5','1',dec_shrub = '6','5',dec_shrub = 'DNR','-1',dec_shrub not in ('','-','0','1.5','6','DNR'),dec_shrub),

dw = switch(dw in ('','-','0'),'NA',dw = '1.5','1',dw = '6','5',dw = 'DNR','-1',dw not in ('','-','0','1.5','6','DNR'),dw),

bare = switch(bare in ('','-','0'),'NA',bare = '1.5','1',bare = '6','5',bare = 'DNR','-1',bare not in ('','-','0','1.5','6','DNR'),bare);


update tblvegtransectpctcover set eq = switch(eq in ('','-','0'),'NA',eq = '1.5','1',eq = '6','5',eq = 'DNR','-1',eq not in ('','-','0','1.5','6','DNR'),eq);



select transectid, habitattype,start,end,gram from tblvegtransectpctcover where transectid = cov2011.transectid and habitattype = cov2011.habitattype and start = cov2011.habitattype and gram not like cov2011.gram;