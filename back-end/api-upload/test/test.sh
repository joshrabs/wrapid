echo "-------------------------------------------------------"
echo "Upload image"
uss=$(curl -i --form file1=@assets/test.png --form press=submit https://api.runabetterset.com/1/upload)
echo "$uss"

echo "-------------------------------------------------------"
echo "Upload skin"
sss=$(curl -i --form payload=@skin-test.csv --form press=submit https://api.runabetterset.com/1/upload/set/runabetterset/skin/2017-03-22)
echo "$sss"

echo "-------------------------------------------------------"
echo "Get skin"
pss=$(curl -i -X GET https://api.runabetterset.com/1/set/runabetterset/skin )
echo "$pss"
