# cesr

__Tools to analyse EuroCES data__

Constant Effort Site (CES) ringing data provide a valuable way to monitor changes in abundance, productivity and survival through standardised capture and recapture of individuals.  cesr provides some facilities to produce annual trends in abundance, productivity and adult survival from these data.  cesr requires CES data organised in a plain text file according to the agreed Euring format (details of which can be found on the Euring website: http://www.euring.org/internal/ces_research_group/euro_ces_meeting_2008_notes.pdf).  

The key steps are:
(i) to read in the data, with readces(); 
(ii) generate a table of which plots are covered in which year, with extract.coverage();
(iii) extract data for the species to be analysed, with extract.data(), and 
(iv) generate the annual abundance/productivity indices, with index(); results can be plotted using plot.trend(). 

Adult survival probabilities can be estimated by first using extract.ch(), then mark.ces().

More details on how to do each of these can be found in the introductory manual ([cesr-intro.pdf](https://drive.google.com/file/d/17025jOoRnyV0UsyMtEq0LOq8dErhDjFL/view?usp=share_link)) and in the helpfiles to each function.
