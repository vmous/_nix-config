#!/bin/zsh

# This is a utility script to launch NICE DCV Viewer window at a predefined
# position in the display.
#
# Currently supports MacOS and uses the `yabai` to get and set the window
# coordinates. Install via Homebrew like so:
#
# ```
# brew install koekeishiya/formulae/yabai
# ```
#
# start its server locally
#
# ```
# yabai --start-service
# ```
#
# If you get an error or prompt related to `yabai`'s Accessibility permissions
# make sure you grant them
#  > System Settings > Privacy & Security > Accessibility > “+” > Yabai
#
# Note: binary found at `/opt/homebrew/bin/yabai`
#
# Lastly, query for window attributes like so:
#
# ```
# yabai -m query --windows | jq '.[] | select(.app | test("dcv"; "i"))'
# ```
# and note down the `frame.*` elements which should be the x, y positions
# of the window along with its width and height.
#
# If you ever want to stop `yabai`, you can do so by stopping its server:
#
# ```
# yabai --stop-service
# ```
#
# and don't forget to go back to Accessibility permission mentioned above and
# turn them off as well.

# Shared shell utilities (cmd_exists, ...). zsh is required both for these and
# for the [[ ]]/local constructs used below.
source "${HOME}/.zsh.d/utils.zsh"

# Function to handle cleanup on SIGINT
cleanup() {
    echo "Terminating DCV client and its children (pgid=${PGID})..."
    kill -- -${PGID} 2>/dev/null
    exit
}

# Trap SIGINT (Ctrl-C) and call cleanup
trap cleanup SIGINT


# Function to display usage information
usage() {
    echo "Usage: ${0} <home-es|home-gr|work>"
    echo "Smart launcher for NICE DCV client"
    echo
    echo "Options:"
    echo "    home-es    Position and scale the client window based on ES home display setup"
    echo "    home-gr    Position and scale the client window based on GR home display setup"
    echo "    work       Position and scale the client window based on work display setup"
    echo
}

# Utilities to display stdout messages in given color
RED="\033[31m"
GREEN="\033[32m"
BLUE="\033[34m"
YELLOW="\033[33m"
RESET="\033[0m"

cecho() {
    # Usage:
    #   cecho "message"
    #   cecho COLOR "message"

    local color msg

    case "$1" in
        "${RED}" | "${GREEN}" | "${BLUE}" | "${YELLOW}")
            color="$1"
            shift
            ;;
        *)
            color="${BLUE}"
            ;;
    esac

    msg="$*"
    printf "%b\n" "${color}${msg}${RESET}"
}

# Validate input arguments
if [[ $# -gt 1 ]]; then
    echo "Too many arguments provided. Doing nothing."
    usage
    exit 1
fi

NICE_DCV_LAUNCH_WRAPPER="python3 /opt/nice-dcv/dcv-cdd.py --debug connect vmous-clouddesk.aka.corp.amazon.com --wssh"
# Check if an active DCV client is already running
if pgrep -f "${NICE_DCV_LAUNCH_WRAPPER}" > /dev/null; then
    cecho "NICE DCV client already running. Nothing to do."
    exit 1
fi

# Start the DCV client in the background
# After running the python command, we capture its process id (PID) and its
# process group id (PGID). We need the PID so that we can wait on the
# process in the end of the script. We need PGID so that we can properly
# terminate all related processes when needed.
cecho "Running: ${NICE_DCV_LAUNCH_WRAPPER}"
eval "${NICE_DCV_LAUNCH_WRAPPER} &" #  Note the '&' in the end!
PID=$!
PGID=$(ps -o pgid= -p ${PID} | tr -d ' ')
cecho "Spinning up parent NICE DCV client process (pid=${PID} / pgid=${PGID})"

if [[ $# -eq 1 ]]; then
    POSITIONING_MODE=$1

    case ${POSITIONING_MODE} in
        home-es)
            POSITION_X=-3830
            POSITION_Y=-1630
            POSITION_WIDTH=3300
            POSITION_HEIGHT=2000
            ;;
        home-gr)
            POSITION_X=1100
            POSITION_Y=2128
            POSITION_WIDTH=3300
            POSITION_HEIGHT=2000
            ;;
        work)
            POSITION_X=-190
            POSITION_Y=-1375
            # 95% of Dell ThinkVision P27h-28's native 2560×1440 (retaining 16:9 ration)
            POSITION_WIDTH=2432
            POSITION_HEIGHT=1367
            ;;
        *)
            cecho "Unknown NICE DCV client window positioning mode: \"${POSITIONING_MODE}\". Ignoring positioning..."
            usage
            # No coordinates to apply; just keep the client running and skip the
            # positioning logic below (which would otherwise use unset presets).
            wait ${PID}
            exit 0
            ;;
    esac

    cecho "Attempting NICE DCV client window positioning (mode: \"${POSITIONING_MODE}\")..."

    if ! cmd_exists yabai || ! yabai -m query --windows >/dev/null 2>&1; then
        cecho "${YELLOW}" "yabai unavailable! Skipping window positioning."
        cecho "${YELLOW}" "Check launcher script documentation on how to install and start the yabai server."
    else
        # The client window can take a few seconds to appear; poll for it.
        WIN_ID=""
        for _ in $(seq 1 30); do
            WIN_ID=$(yabai -m query --windows | jq -r 'map(select(.app|test("dcv";"i")))[0].id // empty')
            [ -n "${WIN_ID}" ] && break
            sleep 1
        done

        if [ -z "${WIN_ID}" ]; then
            cecho "${YELLOW}" "Could not find the DCV client window via yabai; skipping positioning."
        else
            # A native-fullscreen window cannot be moved or resized; exit it first.
            if [ "$(yabai -m query --windows --window ${WIN_ID} | jq -r '.["is-native-fullscreen"]')" = "true" ]; then
                yabai -m window ${WIN_ID} --toggle native-fullscreen
                sleep 2
            fi

            cecho "Positioning NICE DCV client window (id=${WIN_ID}) using coordinates x: ${POSITION_X} y: ${POSITION_Y} w: ${POSITION_WIDTH} h: ${POSITION_HEIGHT}"
            # Always move before resizing (avoids clamping when growing beyond
            # the display edges. Notice that we apply the move+resize twice
            # specifically for clients that use "DCV" -> "Preferences" ->
            # "Display" -> "Display resolution" -> "Adapt automatically"; the
            # first resize makes DCV re-drive the remote head to match, which
            # nudges the window; the second pass lands it on the requested
            # geometry.
            position_dcv_window() {
                yabai -m window ${WIN_ID} --move abs:${POSITION_X}:${POSITION_Y} \
                    && yabai -m window ${WIN_ID} --resize abs:${POSITION_WIDTH}:${POSITION_HEIGHT}
            }
            if position_dcv_window; then
                sleep 2
                position_dcv_window
                cecho "Positioning NICE DCV client window complete!"
            else
                cecho "${RED}" "yabai failed to position the window."
            fi
        fi
    fi
fi

wait ${PID}
