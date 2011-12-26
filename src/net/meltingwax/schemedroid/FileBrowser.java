package net.meltingwax.schemedroid;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import android.app.ListActivity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.ListView;

/***
 * @author John Lombard
 * This class belongs to Jonh Lombard from anddev.org, its a very basic class  but its a good starting point for a filebrowser
 * i made a few changes to make it compatible to 1.6, all the credit belong to him. (-android-codepad developer)
 */
public class FileBrowser extends ListActivity {

	private enum DISPLAYMODE {
		ABSOLUTE, RELATIVE;
	}

	private final DISPLAYMODE displayMode = DISPLAYMODE.RELATIVE;
	private List<String> directoryEntries = new ArrayList<String>();
	private File currentDirectory = new File("/sdcard/");

	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle icicle) {
		super.onCreate(icicle);
		// setContentView() gets called within the next line,
		// so we do not need it here.
		browseToRoot();
		
	}

	/**
	 * This function browses to the root-directory of the file-system.
	 */
	private void browseToRoot() {
		browseTo(new File("/sdcard/"));
	}

	/**
	 * This function browses up one level according to the field:
	 * currentDirectory
	 */
	private void upOneLevel() {
		if (this.currentDirectory.getParent() != null) this.browseTo(this.currentDirectory.getParentFile());
	}

	private void browseTo(final File aDirectory) {
		if (aDirectory.isDirectory()) {
			this.currentDirectory = aDirectory;
			
			String title = aDirectory.getAbsolutePath();
			if (! title.endsWith("/")) {
				title += "/";
			}
			setTitle(title);
			
			fill(aDirectory.listFiles());
		} else {
			Intent resultIntent = new Intent(android.content.Intent.ACTION_VIEW,
					Uri.parse("file://"	+ aDirectory.getAbsolutePath()));
			setResult(RESULT_OK, resultIntent);
			finish();
		}
	}

	private void fill(File[] files) {
		this.directoryEntries.clear();

		try {
			Thread.sleep(10);
		} catch (InterruptedException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		this.directoryEntries.add(".");

		if (this.currentDirectory.getParent() != null)
			this.directoryEntries.add("..");
		if (files != null) {
			switch (this.displayMode) {
				case ABSOLUTE:
					for (File file : files) {
						this.directoryEntries.add(file.getPath());
					}
					break;
				case RELATIVE: // On relative Mode, we have to add the current-path to
								// the beginning
					int currentPathStringLength = this.currentDirectory
							.getAbsolutePath().length();
					for (File file : files) {
						this.directoryEntries.add(file.getAbsolutePath().substring(
								currentPathStringLength));
					}
					break;
			}
		}

		ArrayAdapter<String> directoryList = new ArrayAdapter<String>(this,
				R.layout.file_row, this.directoryEntries);
		
		this.setListAdapter(directoryList);
	}
	
	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		int selectionRowID = (int)l.getItemIdAtPosition(position);
		String selectedFileString = this.directoryEntries.get(selectionRowID);
		if (selectedFileString.equals(".")) {
			// Refresh
			this.browseTo(this.currentDirectory);
		} else if (selectedFileString.equals("..")) {
			this.upOneLevel();
		} else {
			File clickedFile = null;
			switch (this.displayMode) {
			case RELATIVE:
				clickedFile = new File(this.currentDirectory.getAbsolutePath()
						+ this.directoryEntries.get(selectionRowID));
				break;
			case ABSOLUTE:
				clickedFile = new File(this.directoryEntries
						.get(selectionRowID));
				break;
			}
			if (clickedFile != null)
				this.browseTo(clickedFile);
		}
	}
}