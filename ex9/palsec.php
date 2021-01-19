<?php
if ("${_SERVER['QUERY_STRING']}" == "")
  $self = "${_SERVER['PHP_SELF']}";
else
  $self = "${_SERVER['PHP_SELF']}?${_SERVER['QUERY_STRING']}";

if (isset($_GET['cheat']))
  $cheat = true;
else
  $cheat = false;

if (isset($_GET['limit']))
  $limit = $_GET['limit'];
else
  $limit = 10;

session_start();

function palindrome($p) {
 $L = array(); 
 $N = strlen($p);
 for($i = 0; $i <$N; $i++){
	$L[$i][$i]=1;
 }
 for ($cl=2; $cl<=$N; $cl++)
 {
  	for($i = 0; $i<$N-$cl+1; $i++)
	{
		$j = $i + $cl -1;
		if($p[$i]==$p[$j] && $cl ==2){ $L[$i][$j]=2;}	
		elseif ($p[$i] == $p[$j]){
			$L[$i][$j] = $L[$i+1][$j-1] +2;
				
		}else{
			$L[$i][$j] = max($L[$i][$j-1], $L[$i+1][$j]);	
		}
	}
 }	 
 return $L[0][$N-1];
}

function microtime_float()
{
  list($usec, $sec) = explode(" ", microtime());
  return ((float)$usec + (float)$sec);
}

function get_int_max()
{
    $max=0x7fff;
    $probe = 0x7fffffff;
    while ($max == ($probe>>16))
    {
        $max = $probe;
        $probe = ($probe << 16) + 0xffff;
    }
    return $max;
}

if (!defined('PHP_INT_MAX'))
{
    define ('PHP_INT_MAX', get_int_max());
}

$max = array();
$max[0]  = 10;
$max[1]  = 20;
$max[2]  = 30;
$max[3]  = 40;
$max[4]  = 60;
$max[5]  = 80;
$max[6]  = 100;
$max[7]  = 200;
$max[8]  = 600;
$max[9]  = 900;
$max[10] = 1000;
if ($max[10] > 10000000000)
  $max[10] = 10000000000;

function generateRandomString($length = 10) {
    $characters = 'abcdefghijklmnopqrstuvwxyz';
    $charactersLength = strlen($characters);
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, $charactersLength - 1)%($_SESSION['count']*2)];
    }
    return $randomString;
}

function generate() {
  global $max;

  $_SESSION['count']++;
  $num = rand($max[$_SESSION['count']-1],
              $max[$_SESSION['count']]-1);
  //echo "<script type='text/javascript'>alert('$num');</script>";
  $_SESSION['length'] = $num; 
  $_SESSION['string'] = generateRandomString($_SESSION['length']);
  $_SESSION['answer'] = $_SESSION['length'] -  palindrome($_SESSION['string']); 
}

?>

<!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<title>Find the longest palindrome!</title>
<link href="primality.css" rel="stylesheet" type="text/css" />
</head>
<body>

<?php
if (!isset($_SESSION['count']) || isset($_SESSION['reset'])) {
  $_SESSION['count'] = 0;
  $_SESSION['wrong'] = 0;
  $_SESSION['zero'] = microtime_float();
}
if (isset($_SESSION['generate']) || $_SESSION['count'] == 0)
  generate();
unset($_SESSION['generate']);
unset($_SESSION['reset']);
?>

<h1>Find the longest palindrome!</h1>

<h3>
I'll give you a string of (up to 1000) letters and I need you to do one simple thing:

Find the least possible number of letters that, if removed from the given string, what remains is a palindrome.

For example, given the string: bbccaddabaddacaaacdb the correct answer is 5.

If one removes these five underlined letters: bbccaddabaddacaaacdb then the remaining string: bccaddabaddaccb is indeed a palindrome. It is not possible to obtain a palindrome by removing fewer than five letters.
</h3>

<p><span class="question">Question <?php echo "${_SESSION['count']}"; ?> length <?php echo "${_SESSION['length']}"; ?>  </span>
   <br/>
   <span class="number"><?php echo "<code class=\"block\" id=\"question\">${_SESSION['string']}</code>"; ?></span>
   </p>
<table border="0" cellspacing="3">
  <tr>
    <form <?php echo "action=\"$self\""; ?> id="f" name="f" method="post">
      <td width="16">&nbsp;</td>
<?php
      if (isset($_POST['answer'])) {
        if ($_POST['answer'] == $_SESSION['answer']){
          printf("<td><span class=\"right\">Right! :-)</span></td>\n");
            if($_SESSION['count']<$limit){
                
                $_SESSION['generate'] = true;
            }
        }
        else {
          printf("<td><span class=\"wrong\">Wrong! Try again... :-(</span></td>\n");
          //    $_SESSION['count']--;
          $_SESSION['wrong']++;
        }
        if ($_SESSION['count'] < $limit) {
          printf("<td width=\"16\">&nbsp;</td>\n");
          printf("<td><input type=\"submit\" name=\"continue\"
                             id=\"continue\" value=\"Continue!\" /></td>\n");
        }
        else
          $_SESSION['reset'] = true;
      }
      else {
	printf("<input name = \"answer\" type = 'text' >");
        printf("<td><input type=\"submit\" name=\"submit\"
                           id=\"submit\" value=\"Submit!\" /></td>\n");
        if ($cheat) {
          printf("<td width=\"16\">&nbsp;</td>\n");
          printf("<td>if I were you, I'd say %s</td>\n", $_SESSION['answer']);
        }
      }
?>
    </form>
  </tr>
</table>

<?php
  if (isset($_SESSION['reset'])) {
?>
<p><span class="congratulations">Congratulations!</span>
   You answered all questions!</p>
<p>It took you
  <?php printf("%0.3lf", microtime_float() - $_SESSION['zero']); ?> seconds
<?php
  if ($_SESSION['wrong'] == 0) {
?>
  and you made no mistakes.
<?php
  }
  else if ($_SESSION['wrong'] == 1) {
?>
  and you made one mistake
<?php
  }
  else {
?>
  and you made <?php echo "${_SESSION['wrong']}" ?> mistakes.</p>
<?php
  }
?>
<form <?php echo "action=\"$self\""; ?> id="r" name="r" method="post">
<input type="submit" name="again" id="again" value="Play again!" />
</form>
<?php
  }
?>

</body>
</html>
