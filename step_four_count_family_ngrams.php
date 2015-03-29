<?php
/*
 * phpngrammer.php
 */
 
$ngramsbyfamily = array();

$families = array("nondga","Cryptolocker","ConfickerC",    "ConfickerA" ,     "ZeuSGameover" ,   "ConfickerB"  ,    "Pushdo"  , "NewZeuSGameover", "Virut","Bamital","RunForrestRun","Kelihos","Ramdo",        "CoreFlood" , "InfostealerShiz", "Ramnit"  ,        "Shiz"  ,     "Flashback","Sinowal","Dorkbot","Expiro","ZeroAccess","Ransomware","DarkComet"  ,     "Flame"  ,"TinyBanker","Clicker","ExpiroZ","UrlZone","Neverquest");
    
$handle = fopen("familygrams.csv", "r");
if ($handle) {
  $line = fgets($handle);
  $labels = explode(",",trim($line));
  $labelnumbers = array_flip($labels);
  
  
  while (($line = fgets($handle)) !== false) {
 
    // process the line read.
    $linevalues = explode(",",trim($line));
    array_shift($linevalues);
   
    foreach($families as $key => $family) {
      if($linevalues[$labelnumbers["trigram" . $family . "count"]] != 'NA'){
        $ngramsbyfamily["trigram" . $family][(string)$linevalues[$labelnumbers["trigram" . $family]]] = $linevalues[$labelnumbers["trigram" . $family . "count"]];
      }
      if($linevalues[$labelnumbers["fourgram" . $family . "count"]] != 'NA'){
        $ngramsbyfamily["fourgram" . $family][$linevalues[$labelnumbers["fourgram" . $family]]] = $linevalues[$labelnumbers["fourgram" . $family . "count"]];
      }
      if($linevalues[$labelnumbers["fivegram" . $family . "count"]] != 'NA'){
         $ngramsbyfamily["fivegram" . $family][$linevalues[$labelnumbers["fivegram" . $family]]] = $linevalues[$labelnumbers["fivegram" . $family . "count"]];
      }
    }
  }
  
  fclose($handle);
} else {
  die("Error opening file.");
} 

    
foreach($families as $key => $value){
  $ngramsbyfamily["trigram" . $value] = array_slice($ngramsbyfamily["trigram" . $value], 0, 46656/2, TRUE);
}

$output = "";
$urlcount = 0;
$counter = array();
 
/* Parse one or many words for its ngrams. */
$handle = fopen("dgadata3.csv", "r");
if ($handle) {
  $line = fgets($handle);
  $output .= trim($line);
  foreach($ngramsbyfamily as $key => $value){
    $counter[(string)$key] = 0;
    $output .= "," . $key;
  }
  $output .= "\n";
  
  while (($line = fgets($handle)) !== false) {
    $value = explode(",",$line);
    $sld = $value[6];

    $urlcount += 1;
    print("url count: " . $urlcount . " Searching against: " . $sld . "\n");

    $urlcounter = $counter;    
    
    $trigrams = getNgrams($sld,3);
    $fourgrams = getNgrams($sld,4);
    $fivegrams = getNgrams($sld,5);
        
    foreach($families as $key => $value){
      foreach ($trigrams as $k => $gram){
        if(isset($ngramsbyfamily['trigram' . $value][$gram])){ 
          $urlcounter['trigram' . $value] += 1; 
        }
      }
      foreach ($fourgrams as $k => $gram){
        if(isset($ngramsbyfamily['fourgram' . $value][$gram])){ 
          $urlcounter['fourgram' . $value] += 1; 
        }
      }
      foreach ($fivegrams as $k => $gram){
        if(isset($ngramsbyfamily['fivegram' . $value][$gram])){ 
          $urlcounter['fivegram' . $value] += 1; 
        }
      }
    }
 

    
    $output .= trim($line);
    foreach($ngramsbyfamily as $key => $value){
      $output .= "," . $urlcounter[$key];
    }
    $output .= "\n";

  }
  fclose($handle);
} else {
  die("Error opening file.");
} 



/* Write to fs. */
file_put_contents("dgadata4.csv", $output);
      
      
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