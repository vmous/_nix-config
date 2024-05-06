###############################################################################
# A9
###############################################################################

function mdb () {
    GET http://a9-mdb.amazon.com/api/middb >mdb.json
    # Install jq first
    # brew install jq
    jq -C <mdb.json
}
