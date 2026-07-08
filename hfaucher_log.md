# Internship Log: Haley Faucher

## June 1st, 2026
### 11:00 - First Check in with Stacie
- Talked about internship schedule, Stacie will send calendar invites for coworking time and other meetings
- Need to update goals on project document
### 12:00 - Lapenta Orientation
- Introduction to different line office structures
- Mostly administrative stuff, like housing and travel reimbursements

## June 2nd, 2026
### 9:00 - Goals
- Added goals to project document
### 12:00 - Coworking
- Talked about goals for internship, with some updates
- Discussed code review and update of work plan (aim to be complete by June 3rd)
### 14:00 - Code Review (for AtSeaDistribution)
- Within app.R: 
  - redo the logic for species list with a for-loop
    - Be careful, as some species could have more data files than others with different spatial representations
  - Separate UI and server
  - Potentially make the sidebar into a horizontal navigation bar at the top?
  - Remove color palettes, but keep option to make greyscale (unnecessary)
  - Make sure shapes and shapefiles are top layer always on leaflet. Is the ability to disable the layers necessary?
- Within helper_functions.R:
  - Find a better place to store the text strings for the information tabs
- All other fixes that are mentioned in project document already

## June 3rd, 2026
### 9:00 - Blueprint
- Wrote blueprint for first 4 weeks based on code review
### 12:00 - Coast Survey - Data Driven Future (Lorraine Robidoux)
- Deputy Director of the Office of Coast Survey, which is a subset of the NOS
- Navigation products like the ENC (Electronic Navigational Chart) instead of the rasters aligns to mapping in AtSeaDistribution application
### 13:00 - Project Meeting
- With the for-loop approach, other species can have spatio-temporal representation,
  - Paul will provide basic data to test with
  - Try to have some sort of standardization with the data for easier handling
- A couple of information interviews: Erin, Ben, Josh

## June 4th, 2026
### 9:30 - Planning and Programming
- Edited the blueprint a bit, and made notes for the next steps
- Separated the UI and the server and moved the old helper_functions and custom_area_analysis into the global.R for now. 
  - Waiting for coworking to merge into main
- Organized packages on global.R
### 12:00 - How to get the most of the internship
- Make networking a priority, and reach out to people even to talk about non-work related subjects
### 13:00 - Coworking
- Working with GitHub and going over pull request
  - Merged
- Stacie told me about Positron/VSCode so spent most of the time trying to get that working
- Started working on disbaled the generating custom area analysis button until a shapefile(s) has been uploaded
### 14:00 - Integrated Data Analysis Working Group
- Introduced myself
- Listened to others on how I could contribute to their projects

## June 5th, 2026
### 9:00 - Setting Up and Programming
- Got VSCode to work well on my laptop
  - Had to add a load statement on global because VSCode doesn't have sessions that save variables
- Made a branch to disable the generate button until a valid file is uploaded
 - This caused an error as I was testing with an invalid file
  - Solved this by adding an else statement
- Started cleaning the code according to the R linter
  - Only got through UI, but the other ones should *hopefully* be quicker. 
### 12:00 - From the GCRL SFP to Board-Certified Fish Veterinarian (Johnny Shelley, NMFS SIP)
- Mostly about his pathway
  - Emphasized networking and extracurricular research/academics
  
## June 8th, 2026
### 9:00 - Programming
- Merged in pr for disabling the generate analysis button
- Cleaned global.R by removing functions and variables that aren't used
### 12:00 - Domestic fisheries management 101 (Deb Lambert, NMFS)
- Presentation on the three pillars of fisheries service
- Described stock assessment, and saw some parallels to our project
### 13:00 - Coworking
- Worked on cleaning server.R, but messed up at some point so had to revert my changes
  - This should be done by tomorrow, and I should be able to tackle the color palette removal
### 15:00 - Global Monitoring Laboratory "Taking the Pulse of the Planet" (Vanda Grubisic)
- Talked about the research themes of the laboratory
- Described data products, which was cool to hear about

## June 9th, 2026
### 9:00 - Programming
- Cleaned server.R
- Removed all lintr comments from all three files once everything was cleaned
- Waited for Stacie to approve, then merged
### 12:00 - Satellite Observations of Severe Storms (Dan Lindsey, NESDIS)
- Talked about current satellites (GOES-East and GOES-West)
  - Current version (GOES-19 and GOES-16)
  - Future improvements (increased resolution)
### 13:00 - Programming
- Removed color palette options
  - Kept a checkbox for greyscale
- Resolved merge conflicts with this PR
### 15:00 - National Data Buoy Center "Guardians of the Pelagic Sea" (Bill Burnett)
- Described the four different types of data buoys, as well as uncrewed systems and Voluntary Observing Ships
### 16:30 - Coworking
- Discussed looking more into the files and why we have POPhex_MCMC and POPhexagons_sf
  - Going to look more into this tomorrow
- Decided to not meet with Paul tomorrow, as there is not much visual changes to show

## June 10th, 2026
### 9:00 - Solo Work
- Trying to understand the data pipeline a little more
  - Looking through variables in RStudio environment
  - Coming up with a way to change the pipeline to be a bit more efficient
### 12:00 - Global Ocean Monitoring and Observation in NOAA and Beyond (Jess Snowden)
- A good seminar building off of the National Data Buoy Center talk from yesterday
- Also mentioned that you DON'T need a PHD to succeed at NOAA.
### 13:00 - Coworking
- Still working through pipeline
  - Stacie provided me with a code snippet that should make it possible to only load in files as needed.

## June 11th, 2026
### 9:00 - Programming
- Outlined changes to be made to data pipeline
  - Ran into issue with CSV formatting, had to change it
### 13:00 - Coworking
- Did a small demo for Stacie to see if load speed was alright
- After some feedback, moved scale to bottom left, added italicized latin name to map header, and removed the layer controls
  - This may need to be added back after, as shapefiles may not be deleted by trash icon
### 15:00 - Data Management for the Rest of Us (Stacie Koslovsky, AFSC, NMFS, Seattle)
- Some good info on data management (good standards for my work)
- Interesting stuff about detection models
  - Could be a way to expand on my internship contribution

## June 12th, 2026
### 9:00 - Programming
- Merged data-pipeline pr
- Began working on draw tools functionality
  - Had to add a raw species data reactive for calculations
  - Added helper function that generates histogram for shapes and shapefiles
- Currently, rectangle tool works as expected (all calculations correct)
  - Circle does not, as we may need to change the way that the data is gathered from the circle
### 12:00 - Early Career Professional Perspective (Jessica Bunker, Norman OK)
- Talked about CIWRO
- Described ways to grow my career now:
  - Finding a mentor
  - Asking people in my desired field to review my resume
  - Picking a specialty
### 13:15 - Intern Orientations
- Completed Intern and Safety Orientations for UW housing

## June 15th, 2026
### 9:30 - Lapenta Internship Icebreaker
- Got introduced to Cathy Lapenta, Tim Walsh, Ray Tanabe, Dr. Jon Har, Dr. Marian Westly, and Commander Kevin Doremus
### 13:30 - Coworking
- Mostly talking about tools
  - Circles were being calculated with a bounding box
    - This was changed, now has a more accurate calculation
  - Multiple shapes can be added, but only one has current calculation in shape generation tab
    - Need to find a way to clear other shapes, or remove option to add more until it is deleted.

## June 16th, 2026
### 9:00 - Programming
- Found "singleFeature = TRUE" for only added one shape at a time
  - Now when another one is added, the first feature is removed, and the analysis updates for the most recent shape
- Found that adding a shape and then a species doesn't work, so added in code so this works.
- Added edge case in generate_custom_analysis that clears table and histogram if a shape is removed
### 12:00 - NWS Deputy Director Michelle Mainelli Talks with Interns!
- Got a MS and MBA while working for NOAA
  - An option for me
### 13:00 - Coworking
- Decided to add an option for going to specified coordinates
- Updated blueprint and tickets
- Planned meeting with Paul
### 15:00 - NOAA Technology Partnerships Office (Mike Kruk)
- About the TPO and its different directives
  - SBIR
  - Technology Transfer
- Also mentioned some current partnerships they have (good job opportunities)

## June 17th, 2026
### 12:15 - Programming
- Merged draw tools pull request
- Worked on getting shapefile area analysis to work but ran into some bugs
  - Shapefile doesn't update after other fields are edited
  - You cannot remove the shape file
  - Both a shapefile and a drawn shape are visible on the map at the same time
- Started fixing those bugs
  - Allowing shapefile analysis to update if a variable changes
### 13:00 - Project Meeting
- Demo was good, but should look into the collapsing of the tabs again (you have to scroll down quite far to see the analysis)
- Need a way to standardize data, hopefully in a group with the data science stewards
### 13:45 - Programming
- Fixing the rest of the shapefile bugs
  - Made it so only shapefile or shape can be viewed with Javascript call
  - Added a remove button
- Made PR
### 15:00 - Space Weather 101 (Yaireska (Yari) Collado-Vega)
- Intro to Space Weather Prediction Center and Office of Space Weather Observations

## June 18th, 2026
### 9:00 - Programming
- Merged shapefile analysis pr
- Worked on getting file attributes for shapefile download
  - Fixed associated bugs like the download button being available at any time
- Forgot to make a branch so reverted commit and then branched
### 12:00 - Dr. Uccellini Speaks to Lapenta Interns (Weather Ready Nation)
- The steps to building a weather ready nation, and some case studies
### 13:00 - Coworking
- Reviewed and merged pr
- Did some chatting about changes in issues section of GitHub, all resolved now!!

## June 22nd, 2026
### 9:00 - ID Badge pickup
- Picked up ID badge 
### 10:00 - NOAA account and laptop setup
- Set windows and updated computer
- Set NOAA account up after some difficulty
### 11:00 - Solo Set Up
- Read through old gmail
- Emailed Dr. Roohr, so I can be added to the seminar calendar
### 12:30 - Trainings
- Read CLC Welcome Letter
- Did CSAT training
### 14:00 - Installing Services
- Had to contact help desk to install R and Git onto laptop
- Downloaded Positron wihtout any issues
### 15:30 - Programming
- Began adding option for preloaded shapefiles, testing with import
  - Button is there, but there is no server code yet.

## June 23rd, 2026
### 8:00 - Programming
- Added server logic to preloaded shapefiles, with a reactiveVal that holds the path of the file
  - Removed for-loop logic, because only one shapefile can be on the map at all times
### 9:00 - Howard Diamond (OAR) - OAR's Air Resources Laboratory Intro
- Talked about different divisions, and one of the lab's products
  - HYSPLIT
### 10:00 - Programming
- Fixed some more small bugs in the preloaded shapefiles, making sure everything is removed when one is chosen
- Made pr for preloaded shapefiles
- Added an output underneath the map that shows the cell area (static now, but with changing data types, we will see how it changes)
  - Made pr
- Worked on new zoom-to feature
  - The center is set to 208 which was giving some issues 
    - Thought about cleaning data as hexagons were also applied around 208
  - Got latitude to work with a reactive, but longitude was not working as the centering issue was weird
### 14:00 - Walk with Paul
- Walked around campus a bit, and talked about different calculations we will have to do with the different data types
### 15:00 - Programming
- Figured out the longitude issue (typo :/)
  - Made draft pr, as I want Stacie's opinion on the aesthetics of the UI

## June 24th, 2026
### 8:00 - s3 Classes
- Read Hadley textbook about s3 classes
  - Sent Paul a message about how this could look for us
### 8:30 - Merging
- Merged in preloaded shapefiles, and cell-area branches
  - Had to resolve some merge conflicts, but otherwise okay
- Worked on zoom-to-coordinates function, and redesigned the UI a bit
### 10:30 - Chat with Dr. Roohr
- Talked about housing, mentors, presentations, etc
- Also mentioned Direct Hire, which was a great surprise
### 11:00 - Lit Reviews
- Read year-round potential exposure of ringed seals to vessel traffic in the Pacitic Arctic
  - CRAWL could be something good to talk about with Josh during an interview?
- Also read quantifying species-specific aircraft disturbance probability to support pinniped population monitoring and management
### 14:30 - Coworking
- Renamed Download Shapefile to Download Results, and merged in
### 15:00 - New Species Programming
- Was able to get the example data file loaded in
  - Wasn't able to visualize, as its a different format. 
    - My plan here is to create a different script? that visualizes it on the map, and see what overlap there is with what we already have

## June 25th, 2026
### 8:00 - New Species Data
- Trying to find any similarities between the new data and the old data
  - grid_sf is essentially hexagon_sf, so retrieving that should be the same
### 9:00 - Melissa Cook (NMFS) - Uncrewed Aircraft Systems (UAS) to study protected species
- Applied photogrammetry to sperm whale counts and tagging Rice whales in Gulf of Mexico / Mississippi area
### 10:00 - Project Meeting
- Gave Paul a demo of new features and got a few recommendations
  - Make a preloaded shapefile for the Navy area in the Gulf of Alaska
### 10:45 - New Species Data
- Separated grid_sf from the example data
  - Tried to do the same with the N column (actual species data), and it was all zeroes, so the whole map was green
    - But the squares did show up!
  - Paul resent the data with correct values, and it worked
    - Utilized 97% of the CPU to get colors for all cells
    - Made panning VERY slow on the ap
      - Need to find a better way to get the color data, or store the data differently

## June 26th, 2026
### 8:00 - New Species
- The size of the data and the way it is organized is why the app was so slow
  - Reverted commit to work out different data storage strategy
### 9:00 - Brian Montgomery - The 101 of the NWS Operations Center
### 10:00 - New Species Data
- I turned the flat vector with 144720 entries into a matrix
  - Custom area analysis was not working
  - I added NA seasons and years to all other data files to standardize
  - Sent Paul an email to clarify data representation
- Figured out the data organization and was able to display one season at a time (hardcoded)
  - Before it was the overall averages for each cell
  - Got the custom area analysis to not crash and just output the selected abundance (no variance)

## June 29th, 2026
### 8:00 - Emails
- Reading and responding to emails about seminars/networking
  - Intro to a couple cool software products within NWS
### 8:30 - New Species Data
  - Populating season dropdown with correct years and seasons
### 9:00 - NWS Joint EMC/MDL (Hendrik Tolman, Dan Holdaway)
- Process of software dev
  - Current products and trajectory for new ones/updates
### 10:15 - New Species Data
- Got dropdown to work as expected, it's populating dynamically
  - Had issue where it doesn't say the default data being shown
### 10:30 - Monday Meeting
- Decided to make dropdown option into a slider
  - Need to send Paul an email to see if the winters start on the correct dates
    - I.E. winter 2004 starts in 2004, not in 2003
- Aim to finish with slider, custom area calculations, and documentation of adding new species by end of the week
  - Leaving some of Thursday for bug fixes identified by Stacie
### 12:00 - OMAO - Mar/Av/Uxs Operations (LCDR Rachel Pryor, CDR Kevin Doremus, Caitlin Wilson)
- Intro to the NOAA Fleet
  - 15 ships
    - One stationed in New Castle, NH
  - 10 aircraft
### 1:15 - New Species Data
- Making the dropdown into a slider
  - Got functionality working but labeling was a but of a mess
    - Decided to just remove all intermediate labels, but keeping the edge labels and the one currently selected

## June 30th, 2026
### 8:00 - New Species Data
- Reworking columns to represent the chronology better (winter comes before summer)
  - Renaming and reordering winter columns
- Reworking map and legend for temporal so it doesn't flicker as much 
  - Did not get anywhere with this
### 9:00 - Implementing Electronic Monitoring in Alaska's Largest Fishery (Joel Kraski, NMFS, AKRO)
- Talked about different camera systems and how they are helping observers
  - Policy process behind the change
### 9:45 - Email to Kevin Doremus
- Asking for more information of post-storm imagery, as it could be very helpful for my senior thesis
### 10:00 - New Species Data
- Random bug fixes and reworks of stuff I have identified while making changes
  - Figuring out best method for custom area analysis (if else vs. function delegation)
  - Experimenting with what custom area analysis could look like for frequentist data
    - Absolute abundance makes showing a figure difficult, as bearded seals disables this input, making the histogram delete
### 11:30 - Intern Lunch
### 1:30 - Quick Demo for Stacie
- Talked about what we will discuss in our meeting with Paul tommorrow
  - What we want to be shown for the frequentist calculations
### 2:00 - MML Meeting
- Quick introduction of myself and other new interns at the lab
### 2:30 - Documentation for New Species
- Writing the documentation of how adding new species to the app might look
  - Located in Blueprint in Google Docs

## July 1st, 2026
### 7:30 - Emails
- Reading emails and chats
  - Getting up to date on current bugfixes
### 8:00 - Addressing Git Issues
- Giving all Git issues made by Stacie a label and assigning myself to them.
- Making a branch to make the needed changes
  - Some need to be addressed on the new species data branch, so that will be edited on there
  - Other issues I could not duplicate, so I left them for now
  - Still need clarification on the coordinate markers issue
### 10:00 - Meeting with Paul
- Demo of new bearded seal data
- Talking about what we want to display for custom area analysis with absolute abundance
  - As a placeholder, add the abundance number with variance as NA
### 10:30 - Addressing Git Issues and Meeting Feedback
- Pushed commit removing color palette, just greyscale now
- Made output of dates different in new-species-data
### 12:00 - Overview of the National Hurricane Center (Mike Brennan)
### 1:30 - Changing Relative Abundance Labels for Absolute Abundance Species
- Making sure generated shapefile displays the correct label
  - Same for custom area analysis output
### 2:00 - CliftonStrengths Assessment for Lapenta
- Prep for a Lapenta Seminar later this month
### 2:30 - Addressing Meeting Feedback
- Working on abundance panel toggle with absolute abundance data
  - Reworking the has temporal data flag, to include in CSV
    - This will continue into the panel work, as I added another flag for absolute or relative, to make these more separate from the get-go

### July 2nd, 2026
### 8:00 - New Species Data
- Working on getting abundance estimate panel to be unavailable for absolute data
  - Had to make some more changes to csv
- Separating logic for has_temporal and is_relative, as a new dataset could be absolute but not be temporal
### 9:00 - NWS Weather Prediction Center Operations (Dave Novak)
### 10:00 - Quick Merge
- Merging pull request of Stacie's bugs into the main branch and then up to the new specie data branch
  - Ran into some weird stuff on the new species data branch
### 10:30 - Mid-week Check-in
- Talked about getting the merges in
  - More testing by Stacie will be done next week (pushing back harbor seal abundance work)
  - Also some of the problems I ran into
### 11:00 - Integrated Data Analysis Working Group
- Presentation by Devin Johnson about acoustic abundance

### July 6th, 2026
### 8:00 - New Species Data
- Working on miscellaneous things for new species data
  - Making tab system for UI to reduce scrolling
    - Testing
  - Minor UI fixes, renaming, coloring buttons, getting histogram to be conditional
### 10:30 - Monday Meeting
- Talking about UI update I made
  - Potential improvements to be made
  - Bearded seal shows inaccurate histogram due to inputted abundance not cleared after a relative species was picked
    - Have to "maually" reset it to be 1
### 12:00 - Private Industry Opportunities
- Talks from Amy Metz, Bunmi Olukaya, Dave Jones, and Betsy Kling about opportunities outside of government work
### 2:00 - Redoing Data Shape
- Removing seasons and years from list of data (easier indexing)
  - Was unused as the flags for temporal and relative data exist
  - Testing each species' ability to render and have custom area analysis (all worked)
- Updated documentation regarding adding more new species (for maintenance)
### 3:00 - Merge
- Stacie and I decided to merge in the new species data as it currently stands:
  - Not sure when we will decide on custom area analysis output
  - Not sure if we will get new data
- Deleted old branch, and tested on main
  - All new data and UI fixes seem to be present!
### For tomorrow
- Begin to take a look at HarborSealAbundance app while waiting for custom area analysis and new species

## July 7th, 2026
### 8:00 - Paul's Email
- Read through an email Paul sent after doing a stress test
  - Went through each item, and addressed/responded
### 9:00 - Weather Program Office: Research and Life Transitions (Gina Eosco)
### 10:00 - Paul's Email
- Polishing up the last parts of new code and pushing to branch
  - Sent email response
### 10:30 - Harbor Seal App Review
- Briefly looked over ui, server, and global to see any improvements
  - Anything I found was put into my meeting notes document
  - Not much to speed up processing time, as I dont have access to the data yet
### 12:00 - NWS Forecast Operations, Minneapolis St. Paul (Eric Ahasic)
- Cool intro to being a meteorologist/working for NWS
  - Great guy to connect with
### 1:00 - Stress Test with Stacie
- Sat down with Stacie and looked over the app
  - Noting down anything we may want to change
- Also now have access to the data for Harbor Seal app to do better review
### 2:00 - Stress Test Fixes
- Working more on stress test fixes, beginning to look great! 
###  For tomorrow
- Keep making stress test fixes
- Project Check-in with Paul

## July 8th, 2026
- Worked remotely but computer would not connect to the internet, so I had to call in from my personal computer
### 8:30 - Computer Issues
- Tried to get the computer to work. it didn't :/
  - Texted Stacie
### 9:00 - Alek Krautmann (NWS/NASA Spaceflight Met Group and Artemis Support)
### 10:00 - Working on Panels and Markdown file
- Markdown file took like two seconds (new_species.Rmd)
  - Just copied and pasted from the blueprint document we already had.
- Worked on the panels of the how-to page for a while
  - Lots of missing commas caused a lot of app errors, but its working now
  - A basic outline of how to use the application, with descriptions of how the data could be different.
    - Still in the works, need to decide/workshop what to actually say in them.
