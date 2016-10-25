<?php
/*
 * dga-preprocess.php
 * 
 * sld allows a-z0-9 and hyphen
 *
 * trainXwithclass <- traindgadata[,c( "isDGA","sldlen","dword","bigramTRUEratio","trigramTRUEratio","fourgramTRUEratio","fivegramTRUEratio","bigramFALSEratio","trigramFALSEratio","fourgramFALSEratio","fivegramFALSEratio")]
 */


$inputdataindex = 0;
$inputdatafilearray = array(
  "DGA_paperfeatures_nomcr_train.csv",
  "DGA_paperfeatures_nomcr_train_1000.csv",
  "DGA_paperfeatures_nomcr_train_10000.csv",
  "DGA_classification_data_1.csv",
  "DGA_classification_data_1_1000.csv",
  "DGA_classification_data_1_10000.csv",
  "DGA_classification_data_original_parse.csv",
);
$decimalplaces = 6;
 
$time_start = microtime(true); 
 
print("starting dga-preprocess.php\n");


/*
 * Load or build the dwords dictionary.
 */
$buildthedictionary = FALSE;
$dwordarray = array();
if($buildthedictionary){
  $handle = fopen("dwords.txt", "r");
  if ($handle) {
    $header = fgets($handle);
    $linenumber = 0;
    while (($line = fgets($handle)) !== false) {
      $line = trim($line);
      //print("length of line " . $linenumber . " is " . strlen($line) . "\n");
      $linenumber++;
      $words = explode(" ",$line);
      //print("number of words is " . count($words) . "\n");
      foreach($words as $key => $word){
        $word = trim($word);
        $dwordarray[strlen($word)][] = $word;
      }
    }
  }
  unset($dwordarray[0]);
  foreach($dwordarray as $ngram => $list){
    asort($list);
    $dwordarray[$ngram] = array_flip($list);
  }
  file_put_contents("dword_dictionary_array.txt", var_export($dwordarray, TRUE));
  file_put_contents("dword_dictionary_array_serialize.txt", serialize($dwordarray));
} else {
  $text = file_get_contents('dword_dictionary_array_serialize.txt');
  $dwordarray = unserialize($text);
}


// save xten test/train datasets in subdirectory.
$datadirectory = "crosstenvalidationdatasets/";
  
// For x10 validation we pull one tenth out as the test set. Set testslice value from 0 to 9.
for ($testslice = 0; $testslice <= 9; $testslice ++){
  print("processing x-ten slice # " . $testslice . "\n");

  $writehandletrain = fopen($datadirectory . "DGA_data_train_" . $testslice . ".csv", "w");
  $writehandletest = fopen($datadirectory . "DGA_data_test_" . $testslice . ".csv", "w");
  $writehandletestfaulty = fopen($datadirectory . "DGA_data_test_" . $testslice . "_faultyURL.csv", "w");
  fwrite($writehandletrain, "url,isDGA,sld,sldlen,dwordratio\n");
  fwrite($writehandletest, "url,isDGA,sld,sldlen,dwordratio\n");

  // These lists will contain the ngram as the key and the number of occurances as the value.
  $isDGAbilist = array();
  $nonDGAbilist = array();
  $isDGAtrilist = array();
  $nonDGAtrilist = array();
  $isDGAfourlist = array();
  $nonDGAfourlist = array();
  $isDGAfivelist = array();
  $nonDGAfivelist = array();
  
  $dictionary = array();
  for ($i = 2; $i <= 5; $i ++){
    $dictionary[$i]['isDGA'] = new ArrayObject();
    $dictionary[$i]['nonDGA'] = new ArrayObject();
  }


  $handle = fopen($inputdatafilearray[$inputdataindex], "r");
  if ($handle) {
    $header = fgets($handle);
    $linenumber = 0;
    while (($line = fgetcsv($handle)) !== false) {

      // Preprocess James's 14 feature dataset.
      if($inputdataindex == 0 || $inputdataindex == 1 || $inputdataindex == 2){
        // columns specific to DGA_paperfeatures_nomcr_train.csv
        $url = $line[2];
        
        if(TRUE){  // Ignore rows where IP number is in place of URL.
          // check that this url might actually be an IP address
          $pattern = "/[0-9]{1,3}\.[0-9]{1,3}\.([0-9]{1,3})\.[0-9]{1,3}/";
          $found = preg_match_all($pattern, $url, $matches);
          //if($found > 0 && ((string)$url == (string)$matches[1][0])){
          if($found > 0 ){
            fwrite($writehandletestfaulty, $url ."\n");
            continue;
          } 
        }
        
        $isDGA = ($line[1] == "Non-DGA")?0:1;
        
        // Parse valuable parts of url.
        $sld = strtolower($line[2]);
     
        $sld = rtrim($sld,"~.");
        
        $pattern = "/\/.*$/";
        $sld = preg_replace($pattern, "", $sld);
        
        $pattern = "/\.[a-z0-9]{2,4}$/";
        $sld = preg_replace($pattern, "", $sld);
        $sld = preg_replace($pattern, "", $sld);
         
        //$sld = str_replace("-", "", $sld);
        //$sld = str_replace(".", "", $sld);
        $sld = str_replace(array(".","-"), "", $sld);

      } else {  // Use original project style dataset.
        // columns specific to DGA_classification_data_x.csv
        $url = $line[1];  
        $sld = $line[0];
        $isDGA = $line[2];
      }
      
      $sldlen = strlen($sld);
      $dwordratio = round(dword($sld, $dwordarray, TRUE), $decimalplaces);
      $thisline = $url . "," . $isDGA . "," . $sld ."," . $sldlen . ",". $dwordratio . "\n";
      
      if($linenumber % 10 == $testslice){
        // this line reserved for test dataset. Not mined for ngrams.
        fwrite($writehandletest, $thisline);
      } else {
      
      
        for ($i = 2; $i <= 5; $i ++){
          $grams = Ngrams($sld,$i);
          array_walk_recursive($grams, function($key, $item) use (&$dictionary, &$isDGA, $i){
            if($isDGA == 1){
              //$dictionary[$i]['isDGA']->$key = isset($dictionary[$i]['isDGA']->$key) ?  $dictionary[$i]['isDGA']->$key + 1 : 1;
              $dictionary[$i]['isDGA'][$key] = isset($dictionary[$i]['isDGA'][$key]) ?  $dictionary[$i]['isDGA'][$key] + 1 : 1;
            } else {
              //$dictionary[$i]['nonDGA']->$key = isset($dictionary[$i]['nonDGA']->$key) ?  $dictionary[$i]['nonDGA']->$key + 1 : 1;
              $dictionary[$i]['nonDGA'][$key] = isset($dictionary[$i]['nonDGA'][$key]) ?  $dictionary[$i]['nonDGA'][$key] + 1 : 1;
            }
          });
        }
      
        fwrite($writehandletrain, $thisline);
        
      } 
      $linenumber++;
    }
    fclose($handle);
  } else {
    die("Error opening file.");
  } 

  fclose($writehandletrain);
  fclose($writehandletest);

  // New  plan: Build arrays, then cast arrays into ArrayObjects as like:
  // $dictionary['2']['isDGA'] = new ArrayObject($dictionary['2']['isDGA']);
  // And hope that ArrayObject doesn't loose it's way with integers...

           
  // Process those ArrayObjects as necessary.
  for ($i = 2; $i <= 5; $i ++){
    
    // Reverse Sort arrayObject by value.
    $dictionary[$i]['isDGA']->uasort('dsort');
    $dictionary[$i]['nonDGA']->uasort('dsort');
       
    // !! maybe chop list to half of actual length?? so that shorter list doesn't corrupt full length list.
    // Max possible number of ngrams is 37^n. Since we want the 'common' ngrams only, the array_slice truncates the list to contain at most half that number of ngrams.   
    
    $tempisDGA = new ArrayObject();
    $tempnonDGA = new ArrayObject();
    
    $isDGAcounttarget =  min(100000,round(count($dictionary[$i]['isDGA'])/2));
    $nonDGAcounttarget = min(100000,round(count($dictionary[$i]['nonDGA'])/2));
    
    //$dictionary[$i]['isDGA'] = array_slice($dictionary[$i]['isDGA'],0,round((pow(36,$i))/2));
    //$dictionary[$i]['nonDGA'] = array_slice($dictionary[$i]['nonDGA'],0,round((pow(36,$i))/2));

    $counter = 0;
    foreach ($dictionary[$i]['isDGA'] as $ngram => $gramcount){
      if($counter < $isDGAcounttarget){
        //unset($dictionary[$i]['isDGA'][$ngram]);
        $tempisDGA[$ngram] = $gramcount;
      }
      $counter++;
    }
    $counter = 0;
    foreach ($dictionary[$i]['nonDGA'] as $ngram => $gramcount){
      if($counter < $nonDGAcounttarget){
        //unset($dictionary[$i]['nonDGA'][$ngram]);
        $tempnonDGA[$ngram] = $gramcount;
      }
      $counter++;
    }
   
    $dictionary[$i]['isDGA'] = $tempisDGA->getArrayCopy();
    $dictionary[$i]['nonDGA'] = $tempnonDGA->getArrayCopy();
    
    // If an ngram is contained in both lists then it has no predictive value. So it might get removed from both lists.
    foreach($tempisDGA as $gram => $counter) {
      if(isset($tempnonDGA[$gram])) {  
        unset($dictionary[$i]['isDGA'][$gram]);  
        unset($dictionary[$i]['nonDGA'][$gram]);        
      }
    }
    
    unset($tempisDGA);
    unset($tempnonDGA);
        
    print("dictionary[" . $i . "]['isDGA'] truncated to " . count($dictionary[$i]['isDGA']) . "\n"); 
    print("dictionary[" . $i . "]['nonDGA'] truncated to " . count($dictionary[$i]['nonDGA']) . "\n");
    
  }

  // Save the dictionary to the filesystem.
  file_put_contents($datadirectory . "ngramDictionary_" . $testslice . ".txt", print_r($dictionary, TRUE));



  /*
   * Parse train and test dataset files and write new files that contain ngramratios to fs.
   */
  $datafilenamearray = array("DGA_data_train","DGA_data_test");
  foreach ($datafilenamearray as $key => $datafilename){
    print("\nwork on " . $datafilename . "\n");
    // Count ngrams for sld and append to datafile
    $readfilename = $datadirectory .$datafilename . "_" . $testslice . ".csv";
    $readhandletrain = fopen($readfilename, "r");
    $writehandletrain = fopen($datadirectory .$datafilename . "_" . $testslice . "_ngram.csv", "w");
    if ($readhandletrain) {
      $header = fgets($readhandletrain);
      fwrite($writehandletrain, trim($header) . ",bigramisDGAratio,bigramnonDGAratio,trigramisDGAratio,trigramnonDGAratio,fourgramisDGAratio,fourgramnonDGAratio,fivegramisDGAratio,fivegramnonDGAratio\n");
      while (($line = fgetcsv($readhandletrain)) !== false) {
        $sld = $line['2'];
        $sldlen = $line['3'];
        $gramphrase = "";
          
        // isset() is a fast method to check for existance of a gram.
        for ($n = 2; $n <= 5; $n ++){
          $isDGAratio = 0;
          $nonDGAratio = 0;
          if($n <= $sldlen){
            $grams = Ngrams($sld, $n);
            $isDGAcounter = 0;
            $nonDGAcounter = 0;
            foreach($grams as $gram) {
              if(isset($dictionary[$n]['isDGA'][$gram])) {  
                $isDGAcounter += 1;           
              }
              if(isset($dictionary[$n]['nonDGA'][$gram])) {  
                $nonDGAcounter += 1;           
              }
            }
            $isDGAratio =  round($isDGAcounter  /  ($sldlen - $n + 1),$decimalplaces);
            $nonDGAratio =  round($nonDGAcounter  /  ($sldlen - $n + 1),$decimalplaces);
          }
          $gramphrase .= "," . $isDGAratio . "," . $nonDGAratio;
        }  
       
        fwrite($writehandletrain, implode(",", $line) . $gramphrase . "\n");
      }
    }

    fclose($writehandletrain);
    fclose($readhandletrain);
    // remove temporary version of dataset that lacks ngram features
    unlink($readfilename);

  } // end foreach datafilenamearray

  
}  // end for testslice


$time_end = microtime(true);
print("Execution time in seconds: " . ($time_end-$time_start) . "\n");

print("end dga-preprocess.php\n");
 
 
 
function Ngrams($word,$n=3){
  $len=strlen($word);
  $ngram=array();
  for($i=0;$i+$n<=$len;$i++){
    $string="";
    for($j=0;$j<$n;$j++){ 
      $string.=$word[$j+$i]; 
    }
    $ngram[$i]=$string;
  }
  return $ngram;
}


/*
 * This function returns a percentage of the characters in the needle that are contained in smaller words. 
 * A shortcoming is that any numerals in the needle will not be matched.]
 */
 function dword($word,&$dictionary,$givenumeralsapass = TRUE){
  $shortestgram = 3;
  $wordlength = strlen($word);
  if($givenumeralsapass){
    $charbools = preg_replace(array("/[0-9]/","/[a-z]/"),array("1","0"),$word);
    $charbools = str_split($charbools);
  } else {
    $charbools = array_fill(0, $wordlength, 0);
  }
  for ($i = 0; $i <= $wordlength - $shortestgram; $i++){
    for($j = $wordlength - $i; $j >= $shortestgram; $j--){
      $needle = substr($word, $i, $j);
      //print("Starting at character number " . $i . " we are now looking for " .  $needle . " of length " . $j . "\n"); 
      if(isset($dictionary[$j][$needle])) {
        //print("  Found " . $needle . " at " . $dictionary[$j][$needle] . "\n");
        for ($k = $i; $k <= $i+$j-1; $k++){
          $charbools[$k] = 1;
        }
        if(array_sum($charbools) == $wordlength){
          return 1;
        } 
      }
    }
  }
  return array_sum($charbools)/$wordlength;
}

function dsort($a, $b) {
	if($a == $b){ return 0 ; }
	return ($a > $b) ? -1 : 1;
}
