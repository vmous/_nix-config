#!/bin/shell


# Function to restore all modified files except a specified list
#
# This workflow restores all the files in the current working
# directory. It gets a list of files as arguments which excludes from
# the files it restores. This workflow helps in cases when you
# accidentally change a lot of files but you cannot restore the whole
# working directory because some of the files contain changes you wish
# to preserve. In that case you need to be more surgical and be able to
# restore only the files you want. For example imagine you have been
# working on a feature that touched 4 files but at some point you
# accidentally execute your project's linter with a faulty configuration
# which in turn touches 150 files of your project; with this workflow
# you can restore all the files except from the 4 files you were working
# on.
git_j_restore_all_but() {
  if [ -z "$@" ]; then
    echo "Usage: git j-restore-all-but <file1> [<file2> ...]"
    return 1
  fi

  all_modified_files_str=$(git diff --name-only --diff-filter=M)
  local all_modified_files_arr=( $(echo "${all_modified_files_str}") )

  files_to_keep=("$@")

  files_to_restore=()

  for file in "${all_modified_files_arr[@]}"; do
    keep=false
    for keep_file in "${files_to_keep[@]}"; do
      if [ "${file}" = "${keep_file}" ]; then
        keep=true
        break
      fi
    done

    if ! ${keep}; then
      files_to_restore+=("{$file}")
    fi
  done

  total_modified=${#all_modified_files_arr[@]}
  to_restore_count=${#files_to_restore[@]}
  to_keep_count=$((total_modified - to_restore_count))

  echo "--- Git Restore-All-But Summary ---"
  echo "Files to keep:     ${to_keep_count}/${total_modified}"
  for file in "${files_to_keep[@]}"; do
    echo " - ${file}"
  done
  echo ""

  echo "Files to restore:  ${to_restore_count}/${total_modified}"
  for file in "${files_to_restore[@]}"; do
    echo "  - ${file}"
  done
  echo ""


  if [ ${to_restore_count} -gt 0 ]; then
    read -p "Do you want to proceed with restoring these files? (yes/no): " confirmation
    if [[ "$confirmation" =~ ^[Yy][Ee][Ss]$ ]]; then
      echo "Proceeding with restore..."
      CMD="git restore ${files_to_restore[@]}"
      echo "Running: ${CMD}"
      eval ${CMD}
      echo "Restore complete."
    else
      echo "Operation cancelled."
    fi
  else
    echo "No files to restore based on your selection."
  fi
}
