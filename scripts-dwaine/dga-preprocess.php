<?php
/*
 * dga-preprocess.php
 * 
 * DGA data file from James at https://drive.google.com/file/d/0B1Dn5h3H5xylQkNrbU5mZWR2Q00/view?pli=1
 * sld allows a-z0-9 and hyphen
 *
 * trainXwithclass <- traindgadata[,c(
 * "isDGA","sldlen","dword","bigramTRUEratio","trigramTRUEratio","fourgramTRUEratio","fivegramTRUEratio","bigramFALSEratio","trigramFALSEratio","fourgramFALSEratio","fivegramFALSEratio")]
 */

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

  //$handle = fopen("DGA_paperfeatures_nomcr_train_10.csv", "r");
  $handle = fopen("DGA_paperfeatures_nomcr_train.csv", "r");
  if ($handle) {
    $header = fgets($handle);
    $linenumber = 0;
    while (($line = fgetcsv($handle)) !== false) {

      $url = $line[2];  //$trimmed = trim($text, " \t.");
      $isDGA = ($line[1] == "Non-DGA")?0:1;
      $domain = array_reverse(explode ( "." , $url));
      $sld = strtolower($domain[1]);
      $sldlen = strlen($sld);
      $dwordratio = dword($sld, $dwordarray, TRUE);
      $thisline = $url . "," . $isDGA . "," . $sld ."," . $sldlen . ",". $dwordratio . "\n";
      
      if($linenumber % 10 == $testslice){
        // this line reserved for test dataset. Not mined for ngrams.
        fwrite($writehandletest, $thisline);
      } else {
        // Start building the DGAdictionary data.
        $bigrams = Ngrams($sld,2);
        $trigrams = Ngrams($sld,3);
        $fourgrams = Ngrams($sld,4);
        $fivegrams = Ngrams($sld,5);
        
        //http://stackoverflow.com/questions/1496682/how-to-sum-values-of-the-array-of-the-same-key
        array_walk_recursive($bigrams, function($key, $item) use (&$isDGAbilist, &$nonDGAbilist, &$isDGA){
          if($isDGA == 1){
            $isDGAbilist[$key] = isset($isDGAbilist[$key]) ?  $isDGAbilist[$key] + 1 : 1;
          } else {
            $nonDGAbilist[$key] = isset($nonDGAbilist[$key]) ?  $nonDGAbilist[$key] + 1 : 1;
          }
        });
        array_walk_recursive($trigrams, function($key, $item) use (&$isDGAtrilist, &$nonDGAtrilist, &$isDGA){
          if($isDGA == 1){
            $isDGAtrilist[$key] = isset($isDGAtrilist[$key]) ?  $isDGAtrilist[$key] + 1 : 1;
          } else {
            $nonDGAtrilist[$key] = isset($nonDGAtrilist[$key]) ?  $nonDGAtrilist[$key] + 1 : 1;
          }
        });
        array_walk_recursive($fourgrams, function($key, $item) use (&$isDGAfourlist, &$nonDGAfourlist, &$isDGA){
          if($isDGA == 1){
            $isDGAfourlist[$key] = isset($isDGAfourlist[$key]) ?  $isDGAfourlist[$key] + 1 : 1;
          } else {
            $nonDGAfourlist[$key] = isset($nonDGAfourlist[$key]) ?  $nonDGAfourlist[$key] + 1 : 1;
          }
        });
        array_walk_recursive($fivegrams, function($key, $item) use (&$isDGAfivelist, &$nonDGAfivelist, &$isDGA){
          if($isDGA == 1){
            $isDGAfivelist[$key] = isset($isDGAfivelist[$key]) ?  $isDGAfivelist[$key] + 1 : 1;
          } else {
            $nonDGAfivelist[$key] = isset($nonDGAfivelist[$key]) ?  $nonDGAfivelist[$key] + 1 : 1;
          }
        });
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

      


     
  // Assemble lists into a dictionary that is an array of arrays.   
  $dictionary = array();
  $dictionary['2']['isDGA'] = $isDGAbilist;
  $dictionary['2']['nonDGA'] = $nonDGAbilist;
  $dictionary['3']['isDGA'] = $isDGAtrilist;
  $dictionary['3']['nonDGA'] = $nonDGAtrilist;   
  $dictionary['4']['isDGA'] = $isDGAfourlist;
  $dictionary['4']['nonDGA'] = $nonDGAfourlist;
  $dictionary['5']['isDGA'] = $isDGAfivelist;
  $dictionary['5']['nonDGA'] = $nonDGAfivelist;

  // Process those arrays as necessary.
  for ($i = 2; $i <= 5; $i ++){
    // Reverse Sort array by value.
    arsort($dictionary[$i]['isDGA']);
    arsort($dictionary[$i]['nonDGA']);
    // Convert array with ngram as key and count as value to integer key and ngram as value.
    $dictionary[$i]['isDGA'] = array_keys($dictionary[$i]['isDGA']);
    $dictionary[$i]['nonDGA'] = array_keys($dictionary[$i]['nonDGA']);
    // Max possible number of ngrams is 37^n. Since we want the 'common' ngrams only, the array_slice truncates the list to contain at most half that number of ngrams.
    $dictionary[$i]['isDGA'] = array_slice($dictionary[$i]['isDGA'],0,round((pow(37,$i))/2));
    $dictionary[$i]['nonDGA'] = array_slice($dictionary[$i]['nonDGA'],0,round((pow(37,$i))/2));
    // If an ngram is contained in both lists then it has no predictive value. array_diff returns an array that is a subset of the first array without any of the items from the second array.
    $a = array_diff($dictionary[$i]['isDGA'], $dictionary[$i]['nonDGA']);
    $b = array_diff($dictionary[$i]['nonDGA'], $dictionary[$i]['isDGA']);
    $dictionary[$i]['isDGA'] = $a;
    $dictionary[$i]['nonDGA'] = $b;
    // Flip keys/values so that ngram is the key.
    $dictionary[$i]['isDGA'] = array_flip($dictionary[$i]['isDGA']);
    $dictionary[$i]['nonDGA'] = array_flip($dictionary[$i]['nonDGA']);
  }
  //print_r(array_splice($dictionary[3]['nonDGA'],0,20));

  // Save the dictionary to the filesystem.
  file_put_contents($datadirectory . "ngramDictionary_" . $testslice . ".txt", var_export($dictionary, TRUE));





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
        
        /*
        // array_intersect test block
        $gramphrase = "\narray_intersect";
        $onesld_time_start = microtime(true);
        for ($n = 2; $n <= 5; $n ++){
          $isDGAratio = 0;
          $nonDGAratio = 0;
          if($n <= $sldlen){
            $grams = Ngrams($sld, $n);
            $isDGAratio =  round(count(array_intersect($grams, $dictionary[$n]['isDGA']))  /  ($sldlen - $n + 1),2);
            $nonDGAratio = round(count(array_intersect($grams, $dictionary[$n]['nonDGA']))  / ($sldlen - $n + 1),2);
          }
          $gramphrase .= "," . $isDGAratio . "," . $nonDGAratio;
        }  
        $onesld_time_end = microtime(true);
        print("Execution time array_intersect in seconds: " . ($onesld_time_end-$onesld_time_start) . "\n");
        
        
        // in_array test block
        $gramphrase .= "\nin_array       ";
        $onesld_time_start = microtime(true);
        for ($n = 2; $n <= 5; $n ++){
          $isDGAratio = 0;
          $nonDGAratio = 0;
          if($n <= $sldlen){
            $grams = Ngrams($sld, $n);
            $isDGAcounter = 0;
            $nonDGAcounter = 0;
            foreach($grams as $gram) {
              if(in_array($gram,$dictionary[$n]['isDGA'])) {  
                $isDGAcounter += 1;           
              }
              if(in_array($gram,$dictionary[$n]['nonDGA'])) {  
                $nonDGAcounter += 1;           
              }
              //$isDGAratio =  round($isDGAcounter  /  ($sldlen - $n + 1),2);
              //$nonDGAratio =  round($nonDGAcounter  /  ($sldlen - $n + 1),2);
            }
            $isDGAratio =  round($isDGAcounter  /  ($sldlen - $n + 1),2);
            $nonDGAratio =  round($nonDGAcounter  /  ($sldlen - $n + 1),2);
          }
          $gramphrase .= "," . $isDGAratio . "," . $nonDGAratio;
        }  
        $onesld_time_end = microtime(true);
        print("Execution time in_array in seconds: " . ($onesld_time_end-$onesld_time_start) . "\n");
        */
        
        
        // isset test block
        //$gramphrase .= "\nisset          ";
        //$onesld_time_start = microtime(true);
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
              //$isDGAratio =  round($isDGAcounter  /  ($sldlen - $n + 1),2);
              //$nonDGAratio =  round($nonDGAcounter  /  ($sldlen - $n + 1),2);
            }
            $isDGAratio =  round($isDGAcounter  /  ($sldlen - $n + 1),2);
            $nonDGAratio =  round($nonDGAcounter  /  ($sldlen - $n + 1),2);
          }
          $gramphrase .= "," . $isDGAratio . "," . $nonDGAratio;
        }  
        //$onesld_time_end = microtime(true);
        //print("Execution time isset in seconds: " . ($onesld_time_end-$onesld_time_start) . "\n");
        
        
        
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
  return round(array_sum($charbools)/$wordlength,2 );
}