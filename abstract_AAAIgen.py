from lxml import html, etree
import requests
import time
import os.path
import sys

# These characters will be removed from any text inside to keep CSV file consistent
TEXT_DELIMITER = '#' # Text delimiter for csv file
CSV_SEPARATOR = ';' # Item delimiter for csv file
CSV_END_LINE = '\n' # End of entry for csv file

SCRAPE_DELAY_BEFORE = 5.0 # Seconds
SCRAPE_DELAY_RETRY = None # Sets to be equal to SCRAPE_DELAY_BEFORE

def CSV_CLEAN_STRING( s ):
    return s.replace( TEXT_DELIMITER, '' ).replace( CSV_SEPARATOR, '' ).replace( CSV_END_LINE, '' )
#end CSV_CLEAN_STRING

aaai_confs_years = [2017,2016,2015,2014,2013,2012,2011,2010,2008,2007,2006,2005,2004,2002,2000,1999,1998,1997,1996,1994,1993,1992,1991,1990,1988,1987,1986,1984,1983,1982,1980]

def make_fname( s ):
    r = [ '_' if c.isspace() else c for c in s if c.isalnum() or c.isspace() or c == '-' ]
    return ( ''.join( r ) )[:40]
#end make_fname

def find_between(s, first, last):
    try:
        start = s.index( first ) + len( first )
        end = s.index( last, start )
        return s[start:end]
    except ValueError:
        return ""
#end find_between

def find_after(s, ss):
    try:
        start = s.index( ss ) + len( ss )
        return s[start:]
    except ValueError:
        return ""
#end find_between

def is_paper( p ):
    try:
        _,_,_ = p.xpath( 'a' )[0].text, p.xpath( 'i' )[0].text, p.xpath('a')[0].attrib['href']
    except IndexError:
        return False
    return True
#end is_paper

def has_pdf( p ):
    try:
        c = p.getchildren()[1]
        return isinstance( c, html.HtmlComment ) and "pdf" in c.text.rstrip().lower()
    except IndexError:
        return False
    return False
#end is_paper

def get_page( url, delay_before=5.0, delay_retry=None ):
    page = None
    if delay_retry is None:
        delay_retry = delay_before
    #end if
    time.sleep( delay_before )
    while page is None:
        try:
            page = requests.get( url ) #,  headers={'User-Agent':'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:56.0) Gecko/20100101 Firefox/56.0'} )
        except requests.exceptions.RequestException as e:
            print( "Scraping unsucessful {}".format( e ), file=sys.stderr )
            time.sleep( delay_retry )
        #end try
    #end while
    return page
#end is_paper



with open( "{fname}.txt".format( fname = "AAAI" ), mode = 'w', encoding = 'utf-8' ) as conf_file:
    for conf_year in aaai_confs_years:
        conf_paper_id = 1
        print( "\n\n\n{}".format( conf_year ) )
        conf_id = str( conf_year )[-2:]
        conf_url = "https://www.aaai.org/Library/AAAI/aaai{conf_id}contents.php".format( conf_id = conf_id )
        # Scrape nicely
        conf_page = get_page( conf_url, SCRAPE_DELAY_BEFORE, SCRAPE_DELAY_RETRY )
        conf_tree = html.fromstring( conf_page.content )
        conf_page.close()
        conf_tree.make_links_absolute( "https://www.aaai.org/Library/AAAI/" )
        conf_papers = conf_tree.xpath( '//div[@id="box6"]/div[@class="content"]/p[@class="left"]' )
        l = [ ( ( '' if p.xpath( 'a' )[0].text is None else p.xpath( 'a' )[0].text) + ''.join( filter( None, [ subtext.text for subtext in p.xpath( 'a' )[0] ] ) ), p.xpath( 'i' )[0].text, p.xpath('a')[0].attrib['href'], p.getchildren()[1].text if has_pdf( p ) else None ) for p in conf_papers if is_paper( p ) ]
        # Create file
        for paper, author, paper_url, commented_url in l:
            abstract = ''
            if "/paper/view/" in paper_url:
                paper_url = paper_url.replace( 'paper/view', 'paper/viewPaper' )
                if "https://" not in paper_url:
                    paper_url = paper_url.replace( 'http://', 'https://' )
                #end if
                paper_id = "{conf}{year}_{paper_id}".format(
                    conf = find_between( paper_url, 'index.php/', '/' ),
                    year = conf_id,
                    paper_id = find_after( paper_url, '/paper/view/' )
                )
                paper_page = get_page( paper_url, SCRAPE_DELAY_BEFORE, SCRAPE_DELAY_RETRY )
                print( paper_url, paper_page.status_code, paper_page.content[:50] )
                paper_tree = html.fromstring( paper_page.content )
                abstract = "{text}".format( text = paper_tree.xpath( '//div[@id="abstract"]/div' )[0].text )
                paper_page.close()
            elif ".org/Library/" in paper_url:
                if "https://" not in paper_url:
                    paper_url = paper_url.replace( 'http://', 'https://' )
                #end if
                paper_page = get_page( paper_url, SCRAPE_DELAY_BEFORE, SCRAPE_DELAY_RETRY )
                print( paper_url, paper_page.status_code, paper_page.content[:50] )
                paper_tree = html.fromstring( paper_page.content )
                abstract = "{text}".format( text = paper_tree.xpath( '//div[@id="abstract"]/p' )[1].text )
                paper_page.close()
            else:
                pass
            #end if
            paper_id = make_fname( "{:02d}-".format( conf_paper_id ) + paper )
            # Dump to file
            print( "{year}{sep}{td}{id}{td}{sep}{td}{title}{td}{sep}{td}{authors}{td}{sep}{td}{url}{td}{sep}{tda}{abstract}{tda}{endl}".format(
                year     = str(conf_year)[:20],
                id       = str(paper_id)[:20],
                title    = str(paper)[:20],
                authors  = str(author)[:20],
                url      = str(paper_url)[11:],
                abstract = str(abstract)[:20],
                td   = '',
                tda   = '"',
                sep  = '\t',
                endl = '\n')
            )
            conf_file.write( "{year}{sep}{td}{id}{td}{sep}{td}{title}{td}{sep}{td}{authors}{td}{sep}{td}{url}{td}{sep}{td}{abstract}{td}{endl}".format(
                year     = CSV_CLEAN_STRING( str( conf_year ) ),
                id       = CSV_CLEAN_STRING( str( paper_id ) ),
                title    = CSV_CLEAN_STRING( str( paper ) ),
                authors  = CSV_CLEAN_STRING( str( author ) ),
                url      = CSV_CLEAN_STRING( str( paper_url ) ),
                abstract = CSV_CLEAN_STRING( str( abstract ) ),
                td   = TEXT_DELIMITER,
                sep  = CSV_SEPARATOR,
                endl = CSV_END_LINE)
            )
            conf_paper_id += 1
        #end for
    #end for
# end open file