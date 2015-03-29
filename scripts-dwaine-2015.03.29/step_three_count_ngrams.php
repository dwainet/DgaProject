<?php
/*
 * phpngrammer.php
 */
 
 
/* Read from topndramsdf.csv file and parse into arrays */

$topTRUEigrams = array();
$topFALSEigrams = array();
$topTRUEbigrams = array();
$topFALSEbigrams = array();
$topTRUEtrigrams = array();
$topFALSEtrigrams = array();
$topTRUEfourgrams = array();
$topFALSEfourgrams = array();
$topTRUEfivegrams = array();
$topFALSEfivegrams = array();

    
$handle = fopen("topngramsdf.csv", "r");
if ($handle) {
  $line = fgets($handle);
  while (($line = fgets($handle)) !== false) {
    // process the line read.
    $value = explode(",",$line);
    //print("value " . $value[5] . " " . $value[6] . "\n");
    if($value[2] != 'NA'){$topTRUEigrams[] = $value[1];}
    if($value[4] != 'NA'){$topTRUEbigrams[] = $value[3];}
    if($value[6] != 'NA'){$topTRUEtrigrams[] = $value[5];}
    if($value[8] != 'NA'){$topTRUEfourgrams[] = $value[7];}
    if($value[10] != 'NA'){$topTRUEfivegrams[] = $value[9];}
    if($value[12] != 'NA'){$topFALSEigrams[] = $value[11];}
    if($value[14] != 'NA'){$topFALSEbigrams[] = $value[13];}
    if($value[16] != 'NA'){$topFALSEtrigrams[] = $value[15];}
    if($value[18] != 'NA'){$topFALSEfourgrams[] = $value[17];}
    if($value[20] != 'NA'){$topFALSEfivegrams[] = $value[19];}
  }
  fclose($handle);
} else {
  die("Error opening file.");
} 


/* Only look at top half (most common) ngrams for ngram lists that contain all possible combinations.*/
$topTRUEigrams = array_slice($topTRUEigrams, 0, 36/2);
$topFALSEigrams = array_slice($topFALSEigrams, 0, 36/2);
$topTRUEbigrams = array_slice($topTRUEbigrams, 0, 1296/2);
$topFALSEbigrams = array_slice($topFALSEbigrams, 0, 1296/2);
$topTRUEtrigrams = array_slice($topTRUEtrigrams, 0, 46656/2);
$topFALSEtrigrams = array_slice($topFALSEtrigrams, 0, 46656/2);

/* Remove duplicates */
$tti = array_diff($topTRUEigrams, $topFALSEigrams);
$tfi = array_diff($topFALSEigrams, $topTRUEigrams);
$topTRUEigrams = array_flip($tti);
$topFALSEigrams = array_flip($tfi);
$tti = array_diff($topTRUEbigrams, $topFALSEbigrams);
$tfi = array_diff($topFALSEbigrams, $topTRUEbigrams);
$topTRUEbigrams = array_flip($tti);
$topFALSEbigrams = array_flip($tfi);
$tti = array_diff($topTRUEtrigrams, $topFALSEtrigrams);
$tfi = array_diff($topFALSEtrigrams, $topTRUEtrigrams);
$topTRUEtrigrams = array_flip($tti);
$topFALSEtrigrams = array_flip($tfi);
$tti = array_diff($topTRUEfourgrams, $topFALSEfourgrams);
$tfi = array_diff($topFALSEfourgrams, $topTRUEfourgrams);
$topTRUEfourgrams = array_flip($tti);
$topFALSEfourgrams = array_flip($tfi);
$tti = array_diff($topTRUEfivegrams, $topFALSEfivegrams);
$tfi = array_diff($topFALSEfivegrams, $topTRUEfivegrams);
$topTRUEfivegrams = array_flip($tti);
$topFALSEfivegrams = array_flip($tfi);

print("number of TRUE grams i to five: " . count($topTRUEigrams) . "," . count($topTRUEbigrams) . "," . count($topTRUEtrigrams) . "," . count($topTRUEfourgrams) . "," . count($topTRUEfivegrams) . "\n");
print("number of FALSE grams i to five: " . count($topFALSEigrams) . "," . count($topFALSEbigrams) . "," . count($topFALSEtrigrams) . "," . count($topFALSEfourgrams) . "," . count($topFALSEfivegrams) . "\n");


$output = "";
$urlcount = 0;
 
/* Parse one or many words for its ngrams. */
$handle = fopen("dgadata2.csv", "r");
if ($handle) {
  $line = fgets($handle);

  $output .= trim($line). ",igramTRUEcount,bigramTRUEcount,trigramTRUEcount,fourgramTRUEcount,fivegramTRUEcount,igramFALSEcount,bigramFALSEcount,trigramFALSEcount,fourgramFALSEcount,fivegramFALSEcount\n";
  while (($line = fgets($handle)) !== false) {
    $value = explode(",",$line);
    $sld = $value[6];

    $igrams = array();
    $bigrams = array();
    $trigrams = array();
    $fourgrams = array();
    $fivegrams = array();

    $urlcount += 1;
    print("url count: " . $urlcount . " Searching against: " . $sld . "\n");
    /* Count the ngram frequencies. */
    $counter = array('iTRUE'=> 0,'iFALSE' => 0, 'biTRUE'=> 0,'biFALSE' => 0, 'triTRUE'=> 0,'triFALSE' => 0, 'fourTRUE'=> 0,'fourFALSE' => 0,'fiveTRUE'=> 0,'fiveFALSE' => 0, );  
    foreach(getNgrams($sld,1) as $key => $gram) {
      if(isset($topTRUEigrams[$gram])) {        $counter['iTRUE'] += 1;      }
      if(isset($topFALSEigrams[$gram])) {        $counter['iFALSE'] += 1;      }
    }
    foreach(getNgrams($sld,2) as $gram) {
      if(isset($topTRUEbigrams[$gram])) {        $counter['biTRUE'] += 1;      }
      if(isset($topFALSEbigrams[$gram])) {        $counter['biFALSE'] += 1;      }
    }
    foreach(getNgrams($sld,3) as $gram) {
      if(isset($topTRUEtrigrams[$gram])) {        $counter['triTRUE'] += 1;      }
      if(isset($topFALSEtrigrams[$gram])) {        $counter['triFALSE'] += 1;      }
    }
    foreach(getNgrams($sld,4) as $gram) {
      if(isset($topTRUEfourgrams[$gram])) {        $counter['fourTRUE'] += 1;      }
      if(isset($topFALSEfourgrams[$gram])) {        $counter['fourFALSE'] += 1;      }
    }
    foreach(getNgrams($sld,5) as $gram) {
      if(isset($topTRUEfivegrams[$gram])) {        $counter['fiveTRUE'] += 1;      }
      if(isset($topFALSEfivegrams[$gram])) {        $counter['fiveFALSE'] += 1;      }
    }
    
    
    $output .= trim($line) . "," . $counter['iTRUE']  . "," . $counter['biTRUE']. "," . $counter['triTRUE']  . "," . $counter['fourTRUE']."," . $counter['fiveTRUE']. "," . $counter['iFALSE']  . "," . $counter['biFALSE']. "," . $counter['triFALSE']  . "," . $counter['fourFALSE']. "," . $counter['fiveFALSE'] . "\n";

  }
  fclose($handle);
} else {
  die("Error opening file.");
} 


/* Write to fs. */
file_put_contents("dgadata3.csv", $output);
      
      
function getNgrams($match, $n = 3) {
  $ngrams = array();
  $len = strlen($match);
  for($i = 0; $i < $len; $i++) {
          if($i > ($n - 2)) {
                  $ng = '';
                  for($j = $n-1; $j >= 0; $j--) {
                          $ng .= $match[$i-$j];
                  }
                  $ngrams[] = $ng;
          }
  }
  return $ngrams;
}         