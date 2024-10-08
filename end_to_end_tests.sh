if [[ $# < 1 ]]
then 
echo Usage: ./end_to_end_tests.sh [version-number]
exit 1
fi

mkdir integration
version=$1
cp dist-newstyle/build/x86_64-linux/ghc-9.4.8/blastMD-$version/x/blastmd/build/blastmd/blastmd integration
cd integration

function isEmpty() {
	if [[ -z "$@" ]]
	then return 0
	else return 1 
	fi
}

function failTest () {
	echo TEST FAILED
}

echo Blasting with no files at all, expect failure...
./blastmd >/dev/null 2>/dev/null && failTest

echo Checking that blog/ dir was created...
isEmpty $(ls | grep blog) && failTest

mkdir md
mkdir templates
echo head~k1~content~k2~end > templates/head.html
echo top~k1~content~k2~end > templates/top.html
echo bottom~k1~content~k2~end > templates/bottom.html

echo Blasting with no post.schema, expect failure...
./blastmd >/dev/null 2>/dev/null && failTest

echo k1 > post.schema
echo k2 >> post.schema

echo Blasting with no md, expect success...
./blastmd >/dev/null 2>/dev/null || failTest

echo k1: v1 > md/p1.md
echo k2: v2 >> md/p1.md
echo content line 1 >> md/p1.md
echo content line 2 >> md/p1.md

echo Blasting with a valid .md file, expect success...
./blastmd >/dev/null 2>/dev/null || failTest

echo Checking that sitemap.txt was NOT created without url...
isEmpty $( ls blog | grep sitemap.txt ) || failTest

echo Checking that allposts.js was created...
isEmpty $( ls blog | grep allposts.js ) && failTest

echo Checking that blog/p1.html was created...
isEmpty $( ls blog | grep p1.html) && failTest

echo Checking that first replacement was performed...
isEmpty $(cat blog/p1.html | grep v1) && failTest
echo Checking that second replacement was performed...
isEmpty $(cat blog/p1.html | grep v2) && failTest

rm blog/*

echo k1: v1 > md/p2.md

echo Blasting with an additional bad .md file, expect failure...
./blastmd >/dev/null 2>/dev/null && failTest

echo Checking that no .html files made it through...
isEmpty $(ls blog) || failTest

cp md/p1.md md/p2.md
mkdir md/boo
cp md/p1.md md/boo/pooo2.md

echo index > md/index.html
echo https://actual_url.bar/ > base.url
echo Blasting with a corrected second .md file and url...
./blastmd >/dev/null 2>/dev/null || failTest

echo Checking that sitemap.txt was created...
isEmpty $( ls blog | grep sitemap.txt ) && failTest

echo Checking that sitemap.txt has the correct content...
isEmpty $( cat blog/sitemap.txt | grep https://actual_url.bar/boo/pooo2.html ) && failTest && cat blog/sitemap.txt

echo Checking that allposts.js has the correct number of posts...
test $(cat blog/allposts.js | grep _Date | wc -l) -eq 3 || failTest

rm templates/head.html
echo Blasting with missing template file, expect failure...
./blastmd >/dev/null 2>/dev/null && failTest

echo head~k1content~k2~end > templates/head.html
echo Blasting with broken template file, expect failure...
./blastmd >/dev/null 2>/dev/null && failTest

echo "" > templates/head.html
echo Blasting with empty template file, which should still work...
./blastmd >/dev/null 2>/dev/null || failTest

echo Testing Done! Blast\'em!

cd ..
rm -r integration/
