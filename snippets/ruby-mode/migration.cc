#name : change_column
#key : cc
#group : rails.migration
#condition : (rails/migration?)
# --
change_column :${1:`(or (rails/cur-res-title) "table")`}, :${2:column}, :${3:type}
