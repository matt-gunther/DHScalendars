DHScalendars - How to process DHS reproductive calendar data
================
Matt Gunther <mgunther@umn.edu>
28 June, 2021

<!-- README.md is generated from README.Rmd. Please edit that file -->

**Check out the “Quick Start” article to get going as quickly as
possible.**

# What is DHS reproductive calendar data?

Many Individual Recode (IR) datasets contain one or more calendar
strings representing monthly recall data related to women’s reproductive
behavior. Each string consists of approximately 80 single-character
codes, where each character represents a woman’s response to a recall
question repeated for up to approximately 80 months. You’ll find a
robust tutorial on DHS Contraceptive Calendar Data [on the DHS
website](https://www.dhsprogram.com/data/calendar-tutorial/).

Any given sample may contain up to 9 different contraceptive calendars,
but most contain only 1 or 2 calendars. As of this writing, samples have
included calendars on these topics:

-   **Reproductive Events** (common to *all* samples) - For each month,
    indicates whether the woman was pregnant, gave birth, experienced
    termination of a pregnancy, used one of several contraceptive
    methods, or none of the above.
-   **Reasons for Discontinuation of Contraceptive Use** (common to
    *many* samples) - For the final month of continuous use of a
    contraceptive method, indicates the reason why a woman stopped using
    that method of contraception at the time.
-   **Marital Status** - For each month, indicates whether the woman was
    married.
-   **Place of Residence** - For each month, indicates whether the woman
    moved to a new place of residence and, if not, whether her place of
    residence was located in an urban area.
-   **Pregnancy Termination** - For months were a pregnancy was
    terminated, indicates the type of termination (abortion,
    miscarriage, or stillbirth)
-   **Post-partum Amenorrhea** - For months following a pregnancy,
    indicates whether the woman experienced PPA for every month until
    she indicates that she did not experience PPA.
-   **Breastfeeding** - For months following a birth, indicates whether
    the woman breastfed her child.
-   **Post-partum Abstinence** - For months following a pregnancy,
    indicates whether the woman maintained abstinence.
-   **Employment** - For each month, indicates whether the woman was
    employed and, if so, classifies employment type.
-   **Ultrasound** - For each month of a pregnancy, indicates whether
    the woman had an ultrasound.
-   **Source of Family Planning** - Not processed by IPUMS.
-   **Separation** - Not processed by IPUMS.
-   **Source of Abortion Care** - Not processed by IPUMS.

The IR data file does not identify the topic of any calendar. Instead,
each calendar is usually labeled with the prefix `vcal_` followed by a
number ranging from 1-9. The calendar data from one woman in a typical
sample might look something like this:

    ## Rows: 1
    ## Columns: 7
    ## $ caseid <chr> "1"
    ## $ vcal_1 <chr> "        BPPPPPPPP00111111111111111111111111111111TPP0022222222…
    ## $ vcal_2 <chr> "                   2                                  2       …
    ## $ vcal_3 <chr> "        111111111111111111111111111111111111111111111111111111…
    ## $ vcal_4 <chr> "B"
    ## $ vcal_5 <chr> "B"
    ## $ vcal_6 <chr> "B"

Notice that this sample only contains 6 of the possible 9 calendar
variables, and only 3 of these contains responses (you’ll typically see
a single string “B” if the variable was merely included as a
placeholder; it contains no responses). It’s not clear what topics are
represented by `vcal_1`, `vcal_2`, and `vcal_3`, but notice that each of
these strings contains 80 characters including at least 8 empty spaces
on the left; the left-most response character represents the month of
the interview, and the right-most response character represents the
first month in the recall timeline (calendars are padded with spaces on
the left-side, so that that full string length always exceeds the number
of months in the recall timeline - in this case, women were asked to
recall 72 months).

# How do I use this R package?

The purpose of this R package is to provide a simple workflow
accomplishing these tasks for each sample:

1.  Identify the calendars that are included in the IR file
2.  Parse each calendar string
3.  Reshape the calendar data so that each row represents *one month* of
    recall data for each woman (multiply the number of rows in the IR
    file by \~80)
4.  Remove any placeholder rows for months outside of the recall
    timeline
5.  Derive several harmonized variables from each available calendar
6.  Attach placeholder columns for calendar variables that are not
    available for the sample (all values will be `NA`)
7.  Merge derived variables to the original IR file, creating a new LR
    file with a Data Dictionary (new files will be compressed, as they
    will contain duplicated data for all non-calendar variables - one
    duplication for each month in the sample’s calendar)

The documentation you will find here includes:

-   Instructions for processing new samples
-   An explanation and source code for the calculation of each derived
    variable
