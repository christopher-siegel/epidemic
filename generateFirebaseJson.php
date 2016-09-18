<?php
$arr = array();
if (($handle = fopen("magic/R_output.csv", "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, " ")) !== FALSE) {
        // $data[1] // country
        // $data[2] // week
        // $data[4] // percent
        if ($data[2] != "week") {
            $arr["y2016w" . $data[2]][$data[1]]["infected"] = $data[3];
            $arr["y2016w" . $data[2]][$data[1]]["percent"] = $data[4];
        }
    }
    fclose($handle);

    echo json_encode($arr);
}
?>
