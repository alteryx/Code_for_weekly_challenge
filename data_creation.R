dates_df <- read.csv(
  file = 'PSL/week_start_dates.csv'
)
categoryConstants_df <- data.frame(
  Categories = c(
    'Bakery',
    'Dry_and_baking',
    'Dairy',
    'Canned',
    'Frozen',
    'Beverages',
    'Health_and_beauty'
  ),
  Constants = c(
    40,
    32,
    28,
    20,
    16,
    12,
    4
  )
)
yearConstants_df <- data.frame(
  Year = c(
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016
  ),
  Constants = c(
    160,
    320,
    360,
    540,
    600,
    640,
    660
  )
)

storeConstants_df <- data.frame(
  storeID = c(
    'S01',
    'S02',
    'S03',
    'S04',
    'S05'
  ),
  Constants = c(
    4,
    2.4,
    5,
    5.4,
    2.6
  )
)

getAdditiveValueNonProduce <- function(
  in.category_sc,
  in.storeID_sc
){
  categoryConstant_sn = categoryConstants_df[
    categoryConstants_df$Categories == in.category_sc, 
    'Constants'
  ]
  storeConstant_sn = storeConstants_df[
    storeConstants_df$storeID == in.storeID_sc,
    'Constants'
  ]
  return(
    rnorm(
      n = 1,
      mean = categoryConstant_sn * storeConstant_sn,
      sd = sqrt(x = categoryConstant_sn * storeConstant_sn)
    )
  )
}

getYearandMonthDependentValueNonProduce <- function(
  in.year_si,
  in.month_si
){
  yearConstant_si <- yearConstants_df[
    yearConstants_df$Year == in.year_si,
    'Constants'
  ]
  return(
    yearConstant_si / (
      abs(
        x = min(
          10 - in.month_si, 
          2 + in.month_si #We want to compute the current month's distance 
          #from the closest October (either this one or the previous one)
        )
      ) + 1)
  )
}

getSalesTotalNonProduce <- function(
  in.category_sc,
  in.storeID_sc,
  in.year_si,
  in.month_si
){
  return(
    getAdditiveValueNonProduce(
      in.category_sc = in.category_sc,
      in.storeID_sc = in.storeID_sc
    ) + 
      getYearandMonthDependentValueNonProduce(
        in.year_si = in.year_si,
        in.month_si = in.month_si
      )
  )
}

getSalesTotalProduce <- function(
  in.storeID_sc,
  in.year_si,
  in.month_si
){
  monthIndicator_sb <- in.month_si == 10
  return(
    rnorm(
      n = 1,
      mean = 5 * storeConstants_df[
        storeConstants_df$storeID == in.storeID_sc,
        'Constants'
      ],
      sd = sqrt(x = 5 * storeConstants_df[
        storeConstants_df$storeID == in.storeID_sc,
        'Constants'
        ]
      )
    ) + yearConstants_df[
      yearConstants_df$Year == in.year_si, 
      'Constants'
    ] * monthIndicator_sb
  )
}

getSalesTotal <- function(
  in.category_sc,
  in.storeID_sc,
  in.year_si,
  in.month_si
){
  if (in.category_sc == 'Produce'){
    return(
      getSalesTotalProduce(
        in.storeID_sc = in.storeID_sc,
        in.year_si = in.year_si,
        in.month_si = in.month_si
      )
    )
  } else {
    return(
      getSalesTotalNonProduce(
        in.category_sc = in.category_sc,
        in.storeID_sc = in.storeID_sc,
        in.year_si = in.year_si,
        in.month_si = in.month_si
      )
    )
  }
}
sales_df <- dates_df
sales_df$Week_start_dates <- as.POSIXct(x = sales_df$Week_start_dates)
salesByStore_l <- lapply(
  X = list(
    'S01',
    'S02',
    'S03',
    'S04',
    'S05'
  ),
  FUN = function(in.storeID_sc){
    salesTotalsByCategory_l <- lapply(
      X = list(
        'Bakery',
        'Dry_and_baking',
        'Dairy',
        'Canned',
        'Frozen',
        'Beverages',
        'Health_and_beauty',
        'Produce'
      ),
      FUN = function(in.category_sc){
        return(
          mapply(
            FUN = getSalesTotal,
            in.year_si = sales_df$Year,
            in.month_si = sales_df$Month,
            MoreArgs = list(
              in.category_sc = in.category_sc,
              in.storeID_sc = in.storeID_sc
            )
          )
        )
      }
    )
    salesTotalsByCategory_df <- data.frame(salesTotalsByCategory_l)
    colnames(salesTotalsByCategory_df) <- c(
      'Bakery',
      'Dry_and_baking',
      'Dairy',
      'Canned',
      'Frozen',
      'Beverages',
      'Health_and_beauty',
      'Produce'
    )
    return(
      data.frame(
        sales_df,
        salesTotalsByCategory_df
      )
    )
  }
)

#Check to make sure there are no negative sales totals
negativeValuesIndicators_l <- lapply(
  X = salesByStore_l,
  FUN = function(in.sales_df){
    salesColumnsOnly_df <- in.sales_df[
      ,
      !(colnames(in.sales_df) %in% c(
        'Week_start_dates', 
        'Month', 
        'Year'
        )
      )
    ]
    return(any(salesColumnsOnly_df < 0))
  }
)