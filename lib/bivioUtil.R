asdf <- function(y) {
  yc <- xmlChildren(y)
  data.frame(as.id = as.Date(xmlValue(yc$import_date)),
             as.key = xmlValue(yc$sync_key)) # not valuable
}

medf <- function(y) {
  yc <- xmlChildren(y)
  data.frame(
    me.amount = as.numeric(xmlValue(yc$amount)),
    me.type = xmlValue(yc$entry_type),
    me.tax_basis = xmlValue(yc$tax_basis),
    me.tax_category = xmlValue(yc$tax_category),
    me.units = xmlValue(yc$units),
    me.user_id = xmlValue(yc$user_id),
    me.valuation_date = as.Date(xmlValue(yc$valuation_date))
  )
}

aedf <- function(y) {
  yc <- xmlChildren(y)
  data.frame(
    ae.amount = as.numeric(xmlValue(yc$amount)),
    ae.type = xmlValue(yc$entry_type),
    ae.tax_basis = xmlValue(yc$tax_basis),
    ae.tax_category = xmlValue(yc$tax_category),
    ae.allocate = xmlValue(yc$allocate_equally),
    ae.expense_id = xmlValue(yc$expense_id),
    ae.realm_id = xmlValue(yc$realm_account_id)
  )
}

iedf <- function(y) {
  yc <- xmlChildren(y)
  data.frame(
    ie.amount = as.numeric(xmlValue(yc$amount)),
    ie.entry = xmlValue(yc$entry_id),
    ie.type = xmlValue(yc$entry_type),
    ie.tax_basis = xmlValue(yc$tax_basis),
    ie.tax_category = xmlValue(yc$tax_category),
    ie.acquisition_date = as.Date(xmlValue(yc$allocate_equally)),
    ie.count = xmlValue(yc$count),
    ie.external_id = xmlValue(yc$external_identifier),
    ie.realm_id = xmlValue(yc$realm_instrument_id)
  )
}
