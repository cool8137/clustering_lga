var page = require('webpage').create();
  page.open('https://quickstats.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/LGA22310?opendocument', function () {
      console.log(page.content); //page source
      phantom.exit();
  });
