echo "-------------------------------------------------------"
echo "Upload image"
echo " "
uss=$(curl -i --form file1=@test.png --form press=submit localhost:4001/1/upload)
echo "$uss"

echo "-------------------------------------------------------"
echo "Upload skin"
echo " "
sss=$(curl -i --form file1=@skin-test.csv --form press=submit localhost:4001/1/upload/set/runabetterset/skin/2017-03-25)
echo "$sss"

echo "-------------------------------------------------------"
echo "Get skin"
echo " " 
pss=$(curl -i -X GET localhost:4001/1/set/runabetterset/skin )
echo "$pss"

# echo "-------------------------------------------------------"
# echo "Upload avatar"
# echo " "

# echo "-------------------------------------------------------"
# echo "Upload image"
# echo " "

# echo "-------------------------------------------------------"
# echo "Upload image - manipulation scale"
# echo " "

# echo "-------------------------------------------------------"
# echo "Upload image = manipulation rotate"
# echo " "
